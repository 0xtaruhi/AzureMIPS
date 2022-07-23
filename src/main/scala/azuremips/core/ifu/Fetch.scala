package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class IF2ICache(config: CoreConfig) extends Bundle with IMasterSlave {
  val vaddr = UInt(32 bits)
  val vaddr_valid = Bool()
  val paddr = UInt(32 bits)
  val paddr_valid = Bool()
  val instValids = Vec(Bool(), 4)
  val insts = Vec(UInt(32 bits), 4)
  val hit = Bool()

  override def asMaster() {
    in (instValids, insts, hit)
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
    val stall  = in Bool()
    val icache = master(IF2ICache(config))
    val insts  = out(Vec(InstWithPcInfo(), 4))
    val exRedirectEn = in Bool()
    val exRedirectPc = in UInt(32 bits)
  }

  val stage2Redirect   = Bool()
  val stage2RedirectPc = UInt(32 bits)

  val stage0 = new Area {
    val pc = RegInit(U(0, 32 bits))
    val stall = False
    when (io.exRedirectEn) {
      pc := io.exRedirectPc
    } elsewhen (stall) {
      pc := pc
    } elsewhen (stage2Redirect) {
      pc := stage2RedirectPc
    } otherwise {
      pc := pc + 16
    }
    io.icache.vaddr_valid := !stall
    io.icache.vaddr := pc
  }

  val stage1 = new Area {
    val stall  = Bool()
    val filled = Reg(Bool()) init (False)
    val pc = RegNextWhen(stage0.pc, !stall)
    val paddr = UInt(32 bits)

    when (pc(31) === True && pc(30) === False) {
      paddr := U"0" @@ pc(30 downto 0)
    } otherwise {
      paddr := pc
    }
    io.icache.paddr := paddr
    io.icache.paddr_valid := !stall

    when (!stall) {
      when (stage2Redirect || io.exRedirectEn) {
        filled := False
      } otherwise {
        filled := True
      }
      pc := stage0.pc
    }

    when (filled && !io.icache.hit) {
      stall := True
      stage0.stall := True
    } otherwise {
      stall        := False
      stage0.stall := False
    }
  }

  val stage2 = new Area {
    val stall = io.stall
    val pc    = RegNextWhen(stage1.pc, !stall)
    // val filled = RegNextWhen(stage1.filled, !stall)
    import azuremips.core.idu.BranchDecoder
    val branchInfos = for (i <- 0 until 4) yield {
      val branchDecoder = new BranchDecoder
      branchDecoder.io.inst := io.icache.insts(i)
      branchDecoder.io
    }

    val hasBrOrJmp = branchInfos.map(_.isBrOrJmp).reduce(_ || _)
    // val brInstIdx = PriorityMux(for (i <- 0 until 4) yield {
    //   (branchInfos.isBrOrJmp, U(i, 2 bits))
    // })
    val brInstIdx = U(0, 2 bits)
    for (i <- (0 until 4).reverse) {
      when (branchInfos(i).isBrOrJmp) {
        brInstIdx := U(i, 2 bits)
      }
    }


    stage2Redirect := hasBrOrJmp && brInstIdx =/= 3 && branchInfos(brInstIdx).isImmDirectJump
    stage2RedirectPc := (pc + 4 * brInstIdx + 4)(31 downto 28) @@ 
                        branchInfos(brInstIdx).jumpImm(27 downto 0)

    val lastInstIsBrOrJmp = (brInstIdx === U(3)) && hasBrOrJmp
    val brValidMask = Vec(Bool(), 4)
    when (lastInstIsBrOrJmp) {
      brValidMask.init.map(_ := True)
      brValidMask.last := False
    } otherwise {
      when (hasBrOrJmp) {
        for (i <- 0 until 4) {
          brValidMask(i) := Mux(i <= brInstIdx + 1, True, False)
        }
      } otherwise { brValidMask.foreach(_ := True) }
    }

    val validMask = (brValidMask zip io.icache.instValids).map {
      case (a, b) => Mux(!stall, a && b, False)
    }

    for (i <- 0 until 4) {
      io.insts(i).valid := validMask(i)
      io.insts(i).payload := io.icache.insts(i)
      io.insts(i).pc := pc + 4 * i
      io.insts(i).isBr := branchInfos(i).isBrOrJmp 
    }
  }
}

object GenFetchVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Fetch)
  }
}