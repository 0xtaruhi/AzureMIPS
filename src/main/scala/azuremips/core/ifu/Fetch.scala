package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class IF2ICache(config: CoreConfig) extends Bundle with IMasterSlave {
  val vaddr = UInt(32 bits)
  val vaddr_valid = Bool()
  val paddr = UInt(32 bits)
  val paddr_valid = Bool()
  val instValid = Bool()
  val insts = Vec(UInt(32 bits), config.icache.bankNum)
  val hit = Bool()

  override def asMaster() {
    in (instValid, insts, hit)
    out (vaddr, vaddr_valid, paddr, paddr_valid)
  }
}

case class InstWithPcInfo() extends Bundle {
  val valid   = Bool()
  val payload = UInt(32 bits)
  val pc      = UInt(32 bits)
  val isBr    = Bool()
}

class Fetch extends Component {
  val config = CoreConfig()
  val io = new Bundle {
    val stall       = in Bool()
    val icache = master(IF2ICache(config))
    val insts  = out(Vec(InstWithPcInfo(), 4))
    val exRedirectEn = in Bool()
    val exRedirectPc = in UInt(32 bits)
  }

  val stage2Redirect   = Bool()
  val stage2RedirectPc = UInt(32 bits)
  
  val stage0 = new Area {
    val pc     = Reg(UInt(32 bits)) init(U"32'hbfc00000")
    val stall  = False
    when (io.exRedirectEn) {
      pc := io.exRedirectPc
    } elsewhen (stage2Redirect) {
      pc := stage2RedirectPc
    } elsewhen (stall) {
      pc := pc
    } otherwise {
      pc := pc + 16
    }
    val redirect = stage2Redirect || io.exRedirectEn
    io.icache.vaddr_valid := !stall && !redirect
    io.icache.vaddr := pc
  }

  val stage1 = new Area {
    val stall    = Bool()
    val pc       = RegNextWhen(stage0.pc, !stall) init(0)
    val paddr    = UInt(32 bits)
    val redirect = stage2Redirect || io.exRedirectEn
    val addr_valid01 = RegNextWhen(!stage0.stall && !stage0.redirect, !stall) init (False)
    val filled   = True // no tlb here so always hit
    val valid    = RegInit(False)
    when (redirect) {
      valid := False
    } elsewhen (!stall) {
      valid := True
    }

    when (pc(31) === True && pc(30) === False) {
      paddr := U"000" @@ pc(28 downto 0)
    } otherwise {
      paddr := pc
    }
    io.icache.paddr := paddr
    io.icache.paddr_valid := addr_valid01

    when ((!io.icache.hit && addr_valid01 && filled) || io.stall) {
      stall        := True
      stage0.stall := True
    } otherwise {
      stall        := False
      stage0.stall := False
    }
  }

  val stage2 = new Area {
    val stall  = io.stall
    val pc     = RegNext(stage1.pc) init(0)
    val stage1_stall_regnxt = RegNext(stage1.stall) init(False)
    val valid  = RegInit(False)
    when (stage2Redirect || io.exRedirectEn) { // (valid && stage2Redir) || exRedir
      valid := False
    }.otherwise {  //.elsewhen (!stall) {
      valid := stage1.valid
    }
    

    val iCacheInstValids   = Vec(Bool(), config.icache.bankNum)
    iCacheInstValids.foreach(v => v := io.icache.instValid)
    val iCacheInstPayloads = io.icache.insts

    // gen inst valid information
    import azuremips.core.idu.BranchDecoder
    val branchInfos = for (i <- 0 until 4) yield {
      val branchDecoder = new BranchDecoder
      branchDecoder.io.inst := iCacheInstPayloads(i)
      branchDecoder.io
    }

    val hasBrOrJmp = (branchInfos.map(_.isBrOrJmp) zip iCacheInstValids).map(x => x._1 && x._2).reduce(_ || _)
    val brInstIdx = U(0, 2 bits) // find where the br/jr inst is
    for (i <- (0 until 4).reverse) {
      when (branchInfos(i).isBrOrJmp && iCacheInstValids(i)) {
        brInstIdx := U(i, 2 bits)
      }
    }
 
    val lastInstIsBrOrJmp = (brInstIdx === U(3)) && hasBrOrJmp
    val brValidMask = Vec(Bool(), 4)
    when (lastInstIsBrOrJmp) {
      brValidMask.init.map(_ := True)
      brValidMask.last := False
    }.elsewhen (hasBrOrJmp) {
      for (i <- 0 until 4) {
        brValidMask(i) := Mux(i <= (brInstIdx + 1), True, False) // inst after delay slot should be invalid this cycle 
      }
    }.otherwise { brValidMask.foreach(_ := True) }


    val validMask = (brValidMask zip iCacheInstValids).map {
      case (a, b) => Mux(!stage1_stall_regnxt && !io.exRedirectEn && valid, a && b, False)
    }
    // gen inst valid information end, assign inst information to io
    for (i <- 0 until 4) {
      io.insts(i).valid   := validMask(i)
      io.insts(i).payload := iCacheInstPayloads(i)
      io.insts(i).pc      := pc + 4 * i
      io.insts(i).isBr    := branchInfos(i).isBrOrJmp 
    }

    // stage2 Redirection block begin
    // has branch and can jump now
    val branchRedirectEn = hasBrOrJmp && brInstIdx =/= 3 && branchInfos(brInstIdx).isImmDirectJump
    val branchRedirectPc = (pc + 4 * brInstIdx + 4)(31 downto 28) @@ branchInfos(brInstIdx).jumpImm(27 downto 0)
    // not all 4 insts are valid this cycle due to valid masks, so we need redirect to fetch them again
    val invInstIdx = U(0, 2 bits)
    for (i <- (0 until 4).reverse) {
      when (!validMask(i)) {
        invInstIdx := U(i, 2 bits)
      }
    }
    val invalidRedirectEn = !validMask.reduce(_ && _) && validMask.reduce(_ || _)
    val invalidRedirectPc = (pc + 4 * invInstIdx)

    stage2Redirect   := (branchRedirectEn || invalidRedirectEn) && valid
    stage2RedirectPc := Mux(branchRedirectEn, branchRedirectPc, invalidRedirectPc)
    // stage2 Redirection block end
  }
}

object GenFetchVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Fetch)
  }
}