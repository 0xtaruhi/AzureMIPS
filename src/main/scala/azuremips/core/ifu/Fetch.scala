package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.ifu.bpu._
import azuremips.core.mmu.{Mmu, TranslateAddrReq}

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
  val isNop   = Bool()
  val predictTarget = UInt(32 bits)
}

class Fetch extends Component {
  val config = CoreConfig()
  val io = new Bundle {
    val stall       = in Bool()
    val icache = master(IF2ICache(config))
    val insts  = out(Vec(InstWithPcInfo(), 4))
    val exRedirectEn = in Bool()
    val exRedirectPc = in UInt(32 bits)
    val cp0RedirectEn = in Bool()
    val cp0RedirectPc = in UInt(32 bits)

    // Branch Prediction
    val updatePc     = in UInt(32 bits)
    val updateEn     = in Bool()
    val updateTaken  = in Bool()

    // Tlb
    val tlbPort      = master(TranslateAddrReq())

    // ICache Inst
    val icacheInstValid = in Bool()
    val icacheInstVAddr = in UInt(32 bits)
  }

  val stage2Redirect   = Bool()
  val stage2RedirectPc = UInt(32 bits)

  val bht = Bht()
  val btb = Btb()

  val mmu = Mmu()
  
  val stage0 = new Area {
    val stall = False
    val pc     = Reg(UInt(32 bits)) init(U"32'hbfc00000")
    val pcMmu  = RegNextWhen(pc, !stall) init(U"32'hbfc00000")
    when (io.cp0RedirectEn) {
      pc := io.cp0RedirectPc
    } elsewhen (io.exRedirectEn) {
      pc := io.exRedirectPc
    } elsewhen (stage2Redirect) {
      pc := stage2RedirectPc
    } elsewhen (stall) {
      pc := pc
    } otherwise {
      pc := pc + 16
    }
    val redirect = stage2Redirect || io.exRedirectEn || io.cp0RedirectEn
    val addrValid = Bool()
    when (io.cp0RedirectEn) {
      addrValid := False
    } elsewhen (io.exRedirectEn && io.icacheInstValid) {
      addrValid := True
    } otherwise {
      addrValid := !(stage2Redirect || io.exRedirectEn)
    }
    io.icache.vaddr_valid := !stall && addrValid
    io.icache.vaddr := Mux(io.icacheInstValid && !io.cp0RedirectEn, io.icacheInstVAddr, pc)

    // mmu
    mmu.io.vaddr := pcMmu
    mmu.io.is_write := False
    io.tlbPort.vpn := mmu.io.tlbPort.vpn
  }

  val stage1 = new Area {
    val stall    = Bool()
    val pc       = RegNextWhen(stage0.pc, !stall) init(0)
    val tlbPfn   = RegNextWhen(io.tlbPort.pfn, !stall) init(0)
    val tlbFound = RegNextWhen(io.tlbPort.found, !stall) init(False)
    val tlbValid = RegNextWhen(io.tlbPort.valid, !stall) init(False)
    val tlbCache = RegNextWhen(io.tlbPort.cache, !stall) init(False)
    val tlbDirty = RegNextWhen(io.tlbPort.dirty, !stall) init(False)
    val paddr    = UInt(32 bits)
    val redirect = stage2Redirect || io.exRedirectEn || io.cp0RedirectEn
    val addr_valid01 = RegNextWhen(!stage0.stall && stage0.addrValid, !stall) init (False)
    val filled   = True // no tlb here so always hit
    val branchRedirectPcPkg = Vec(UInt(32 bits), config.icache.bankNum)
    val instPcPkg = Vec(UInt(32 bits), 5)
    for (i <- 0 until config.icache.bankNum) {
      branchRedirectPcPkg(i) := pc + 4 * i + 8
    }
    for (i <- 0 until 5) {
      instPcPkg(i) := pc + 4 * i
    }
    val valid = RegInit(False)
    when (redirect) {
      valid := False
    } elsewhen (!stall) {
      // valid := stage0.valid
      valid := True
    }

    // when (pc(31) === True && pc(30) === False) {
    //   paddr := U"000" @@ pc(28 downto 0)
    // } otherwise {
    //   paddr := pc
    // }
    
    // mmu.io.vaddr := pc
    // mmu.io.is_write := False
    // mmu.io.tlbPort <> io.tlbPort
    mmu.io.tlbPort.pfn   := tlbPfn
    mmu.io.tlbPort.found := tlbFound
    mmu.io.tlbPort.valid := tlbValid
    mmu.io.tlbPort.cache := tlbCache
    mmu.io.tlbPort.dirty := tlbDirty
    paddr := mmu.io.paddr
    
    io.icache.paddr := paddr
    io.icache.paddr_valid := addr_valid01

    when ((!io.icache.hit && addr_valid01 && filled) || io.stall) {
      stall        := True
      stage0.stall := True
    } otherwise {
      stall        := False
      stage0.stall := False
    }

    // Branch Prediction
    bht.io.pc       := pc
    bht.io.updateEn := io.updateEn
    bht.io.updatePc := io.updatePc
    bht.io.updateTaken := io.updateTaken
    btb.io.vaddr1   := pc
    btb.io.vaddr2   := pc(31 downto config.icache.tagLoUpperBound+1) @@
         (pc(config.icache.tagLoUpperBound downto 3) + U(1)) @@ pc(2 downto 0)
    btb.io.updatePc := io.updatePc
    btb.io.updateEn := io.exRedirectEn
    btb.io.actualTarget := io.exRedirectPc
  }

  val stage2 = new Area {
    val stall  = io.stall
    val pc     = RegNextWhen(stage1.pc, !stall) init (0)
    val branchRedirectPcPkg = Vec(RegInit(U(0, 32 bits)), config.icache.bankNum)
    val instPcPkg = Vec(RegInit(U(0, 32 bits)), 5)
    for(i <- 0 until config.icache.bankNum) {
      when (!stall) { branchRedirectPcPkg(i) := stage1.branchRedirectPcPkg(i) }
    }
    for (i <- 0 until 5) {
      when (!stall) { instPcPkg(i) := stage1.instPcPkg(i) }
    }
    
    val stage1_stall_regnxt = RegNext(stage1.stall) init(False)
    val valid  = RegInit(False)
    when ((valid && stage2Redirect) || io.exRedirectEn || io.cp0RedirectEn) { // (valid && stage2Redirect) || exRedir
      valid := False
    } elsewhen (!stall) {
      valid := stage1.valid
    }
    val iCacheInstValids_now   = Vec(Bool(), config.icache.bankNum)
    iCacheInstValids_now.foreach(v => v := io.icache.instValid)
    
    val holdICacheInstValids = Vec(Reg(Bool()) init(False), 4)
    val holdICacheInstPayloads = Vec(Reg(UInt(32 bits)) init(U"32'h0"), 4)
    val haveStalled = Reg(Bool()) init(False) // hold icache valid signals
    when (stall.rise) {
      holdICacheInstValids   := iCacheInstValids_now
      holdICacheInstPayloads := io.icache.insts
      haveStalled := True
    } 
    when (stall.fall) {
      holdICacheInstValids.foreach(_ := False)
      haveStalled := False
    }

    val iCacheInstValids   = Mux(haveStalled, holdICacheInstValids, iCacheInstValids_now)
    val iCacheInstPayloads = Mux(haveStalled, holdICacheInstPayloads, io.icache.insts)
    // val iCacheInstValids   = Vec(Bool(), config.icache.bankNum)
    // iCacheInstValids.foreach(v => v := io.icache.instValid)
    // val iCacheInstPayloads = io.icache.insts

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
    // stage2 Redirection block begin
    // ras
    val ras = Ras(depth = config.ifConfig.rasDepth)
    val rasRedirectEn = branchInfos(brInstIdx).isReturn && ras.io.topValid && validMask(brInstIdx) // iCacheInstValids.orR && brInstIdx =/= U(3)
    ras.io.pushEn   := branchInfos(brInstIdx).isCall && validMask(brInstIdx)
    ras.io.popEn    := branchInfos(brInstIdx).isReturn && ras.io.topValid && validMask(brInstIdx)
    val rasPushPc = UInt(32 bits)
    switch (brInstIdx) {
      is (U(0)) { rasPushPc := instPcPkg(2) }
      is (U(1)) { rasPushPc := instPcPkg(3) }
      default   { rasPushPc := instPcPkg(4) }
    }
    ras.io.pushData := rasPushPc
    val rasRedirectPc = ras.io.topData
    ras.io.flush := False

    // has branch and can jump now
    val branchRedirectEn = hasBrOrJmp && brInstIdx =/= 3 && branchInfos(brInstIdx).isImmDirectJump
    val branchRedirectPc = branchRedirectPcPkg(brInstIdx)(31 downto 28) @@ branchInfos(brInstIdx).jumpImm(27 downto 0)
    // not all 4 insts are valid this cycle due to valid masks, so we need redirect to fetch them again
    val invInstIdx = U(0, 2 bits)
    for (i <- (0 until 4).reverse) {
      when (!validMask(i)) {
        invInstIdx := U(i, 2 bits)
      }
    }
    val invalidRedirectEn = !validMask.reduce(_ && _) && validMask.reduce(_ || _)
    val invalidRedirectPc = instPcPkg(invInstIdx.resize(3)) // (pc + 4 * invInstIdx)

    // Branch Prediction
    // val takeUpperBtb = !pc(3) || (!pc(2) && brInstIdx(0)) || (!brInstIdx(0) && !brInstIdx(1))
    val takeUpperBtb = (brInstIdx === 0) || (brInstIdx === 1 && !pc(2))
    val btbHit = Mux(takeUpperBtb, btb.io.btbHit1, btb.io.btbHit2)
    val btbBpTarget = Mux(takeUpperBtb, btb.io.predictTarget1, btb.io.predictTarget2)
    val bpRedirectEn = hasBrOrJmp && validMask(brInstIdx) && bht.io.predictTaken && btbHit
    val bpRedirectPc = btbBpTarget

    stage2Redirect   := (branchRedirectEn || invalidRedirectEn || rasRedirectEn || bpRedirectEn) && valid
    // stage2RedirectPc := Mux(branchRedirectEn, branchRedirectPc, invalidRedirectPc)
    when (branchRedirectEn) {
      stage2RedirectPc := branchRedirectPc
    } elsewhen (rasRedirectEn) {
      stage2RedirectPc := rasRedirectPc
    } elsewhen (bpRedirectEn) {
      stage2RedirectPc := bpRedirectPc
    } otherwise {
      stage2RedirectPc := invalidRedirectPc
    }
    // stage2 Redirection block end
    val predictTargetPc = UInt(32 bits)
    when (branchRedirectEn) {
      predictTargetPc := branchRedirectPc
    } elsewhen (rasRedirectEn) {
      predictTargetPc := rasRedirectPc
    } elsewhen (bpRedirectEn) {
      predictTargetPc := bpRedirectPc
    } otherwise {
      predictTargetPc := rasPushPc
    }

    for (i <- 0 until 4) {
      io.insts(i).valid         := validMask(i)
      io.insts(i).payload       := iCacheInstPayloads(i)
      io.insts(i).pc            := instPcPkg(i)
      io.insts(i).isBr          := branchInfos(i).isBrOrJmp
      io.insts(i).isNop         := !iCacheInstPayloads(i).orR
      io.insts(i).predictTarget := predictTargetPc
    }
  }
}

object GenFetchVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Fetch)
  }
}