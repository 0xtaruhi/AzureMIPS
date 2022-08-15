package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._
import azuremips.core.cache.{DReq, DResp, CReq, CacheInstInfo}
import azuremips.core.Uops._
import azuremips.core.exu.ExecutedSignals
import azuremips.core.reg.WriteGeneralRegfilePort
import azuremips.core.cp0.{Cp0ReadPort, Cp0WritePort, ExptInfo, ExptReq}
import azuremips.core.mmu.Mmu
import azuremips.core.mmu.TranslateAddrReq

class DCachePort extends Bundle with IMasterSlave {
  val req = new DReq()
  val rsp = new DResp()

  override def asMaster(): Unit = {
    out(req)
    in(rsp)
  }
}

class MemCachePort extends Bundle with IMasterSlave {
  val req = new Bundle {
    val vaddr_valid  = Bool()
    val vaddr        = UInt(32 bits)
    val strobe       = UInt(4 bits)
    val size         = UInt(3 bits)
    val data         = UInt(32 bits)
    // val isICacheInst = Bool()
    val isDCacheInst = Bool()
    val cacheOp      = UInt(2 bits)
    val uncache      = Bool()
    val paddr        = UInt(32 bits)
    val reqValid     = Bool()
  }
  val rsp = new Bundle {
    val hit         = Bool()
    val data        = UInt(32 bits)
  }
  // val exptValid     = Bool()
  // val exptCode      = UInt(exptCodeWidth bits)
  override def asMaster(): Unit = {
    out(req)
    in(rsp)
  }
}

case class MmuSignals() extends Bundle {
  val vaddr = UInt(32 bits)
  val vaddr_valid = Bool()
  val uncache = Bool()
  val paddr = UInt(32 bits)
  val exptValid = Bool()
  val exptCode = UInt(exptCodeWidth bits)
  val reqValid = Bool()

  def nopMmuSignals() = {
    val m = MmuSignals()
    m.vaddr       := 0
    m.vaddr_valid := False
    m.uncache     := False
    m.paddr       := 0
    m.exptValid   := False
    m.exptCode    := 0
    m.reqValid    := False
    m
  }
}

class SingleMem extends Component {
  val io = new Bundle {
    val executedSignals = in(new ExecutedSignals) // delayed now
    val stall           = in Bool()
    val mmuSignals      = in(new MmuSignals)
    val dcache          = master(new MemCachePort)
    val cacheMiss       = out Bool()
    val mem1Bypass      = out(new BypassPort)
    val mem2Bypass      = out(new BypassPort)
    val mem3Bypass      = out(new BypassPort)
    val wrRegPort       = master(new WriteGeneralRegfilePort)
    val rdCp0Data       = in UInt(32 bits)
    // val wrCp0Port       = master(new Cp0WritePort)
    val hwIntTrig       = in Bool()
    val except          = out(ExptInfo())
  }

  val stage1 = new Area {
    val isLoad  = io.executedSignals.rdMemEn && !io.hwIntTrig
    val isStore = io.executedSignals.wrMemEn && !io.hwIntTrig
    val isICacheInst = io.executedSignals.isICacheInst && !io.hwIntTrig
    val isDCacheInst = io.executedSignals.isDCacheInst && !io.hwIntTrig
    val isCacheInst = isICacheInst || isDCacheInst
    
    // io.dcache.req.vaddr       := io.executedSignals.memVAddr
    // val isUnalinedLS = Bool()
    // switch (io.executedSignals.uop) {
    //   is (uOpSwl, uOpSwr, uOpLwl, uOpLwr) {
    //     isUnalinedLS := True
    //   }
    //   default { isUnalinedLS := False }
    // }
    // io.dcache.req.vaddr := io.executedSignals.memVAddr(31 downto 2) @@ Mux(
    //   isUnalinedLS, U(0, 2 bits), io.executedSignals.memVAddr(1 downto 0)
    // )
    io.dcache.req.vaddr      := io.mmuSignals.vaddr
    io.dcache.req.vaddr_valid := io.mmuSignals.vaddr_valid
    io.dcache.req.data        := io.executedSignals.wrData
    io.dcache.req.strobe      := io.executedSignals.wrMemMask
    io.dcache.req.size        := io.executedSignals.memSize
    // io.dcache.req.isCacheInst := isCacheInst
    // switch (io.executedSignals.uop) {
    //   is (uOpDCacheHI)  { io.dcache.req.cacheOp := CacheInstInfo.HIT_INVALID }
    //   is (uOpDCacheHWI) { io.dcache.req.cacheOp := CacheInstInfo.HIT_INVALID_WB }
    //   is (uOpDCacheIWI) { io.dcache.req.cacheOp := CacheInstInfo.INDEX_INVALID_WB }
    //   is (uOpDCacheIST) { io.dcache.req.cacheOp := CacheInstInfo.INDEX_STORE }
    //   default { io.dcache.req.cacheOp := 0 }
    // }
    // io.dcache.req.isICacheInst := io.executedSignals.isICacheInst
    io.dcache.req.isDCacheInst := io.executedSignals.isDCacheInst
    io.dcache.req.cacheOp      := io.executedSignals.cacheOp
    io.dcache.req.uncache      := io.mmuSignals.uncache
    io.dcache.req.paddr        := io.mmuSignals.paddr
    io.dcache.req.reqValid     := io.mmuSignals.reqValid

    io.mem1Bypass.wrRegEn     := io.executedSignals.wrRegEn && !io.hwIntTrig
    io.mem1Bypass.wrRegAddr   := io.executedSignals.wrRegAddr
    io.mem1Bypass.wrData      := io.executedSignals.wrData
    io.mem1Bypass.isLoad      := isLoad || io.executedSignals.rdCp0En

    val memSize = io.executedSignals.memSize
    val signExt = io.executedSignals.signExt

    io.except.exptValid := (isLoad || isStore || isCacheInst) && io.mmuSignals.exptValid
    io.except.exptCode  := io.mmuSignals.exptCode
    io.except.eret      := False

  }

  val stage2 = new Area {
    val isLoad  = RegInit(False)
    val isStore = RegInit(False)
    val isDCacheInst = RegInit(False)
    val executedSignals = RegInit(ExecutedSignals().nopExecutedSignals)

    when (io.stall) {
      isLoad := isLoad
      isStore := isStore
      isDCacheInst := isDCacheInst
      executedSignals := executedSignals
    }.elsewhen (io.hwIntTrig || io.except.exptValid) {
      isLoad  := False
      isStore := False
      isDCacheInst := False
      executedSignals := ExecutedSignals().nopExecutedSignals
    }.otherwise {
      isLoad  := stage1.isLoad
      isStore := stage1.isStore
      isDCacheInst := stage1.isDCacheInst
      executedSignals := io.executedSignals
      when (io.executedSignals.rdCp0En) {
        executedSignals.wrData := io.rdCp0Data
      }
    }

    when ((isLoad || isStore || isDCacheInst) && !io.dcache.rsp.hit) {
      io.cacheMiss := True
    } otherwise { io.cacheMiss := False }

    io.mem2Bypass.wrRegEn     := executedSignals.wrRegEn
    io.mem2Bypass.wrRegAddr   := executedSignals.wrRegAddr
    io.mem2Bypass.wrData      := executedSignals.wrData
    io.mem2Bypass.isLoad      := isLoad || executedSignals.rdCp0En

    val signExt = RegNextWhen(stage1.signExt, !io.stall) init (False)
    val memSize = RegNextWhen(stage1.memSize, !io.stall) init (0)
  }

  val stage3 = new Area {
    val executedSignals = Reg(new ExecutedSignals) init (ExecutedSignals().nopExecutedSignals)
    val isLoad          = Reg(Bool()) init (False)
    val isStore         = Reg(Bool()) init (False)
    val signExt         = Reg(Bool()) init (False)
    val memSize         = Reg(UInt(3 bits)) init (0)
    
    when (!io.stall) {
      executedSignals := stage2.executedSignals
      isLoad          := stage2.isLoad
      isStore         := stage2.isStore
      signExt         := stage2.signExt
      memSize         := stage2.memSize
    } otherwise {
      executedSignals := ExecutedSignals().nopExecutedSignals
      isLoad          := False
      isStore         := False
      signExt         := False
      memSize         := 0
    }

    io.wrRegPort.wrEn    := executedSignals.wrRegEn
    io.wrRegPort.pc      := executedSignals.pc
    val dcacheRspData    = io.dcache.rsp.data
    // align dcacheRspData
    val readDataAlignInst = new ReadDataAlign()
    readDataAlignInst.io.size := memSize
    readDataAlignInst.io.is_signed := signExt
    readDataAlignInst.io.addr10 := executedSignals.memVAddr(1 downto 0)
    readDataAlignInst.io.raw_data := dcacheRspData
    readDataAlignInst.io.op :=  executedSignals.uop
    readDataAlignInst.io.original := executedSignals.wrData

    val wrData = UInt(32 bits)
    when (executedSignals.rdMemEn) {
      wrData := readDataAlignInst.io.data_o
    } elsewhen (executedSignals.uop === uOpSc) {
      wrData := 1
    } otherwise {
      wrData := executedSignals.wrData
    }

    io.wrRegPort.data    := wrData
    io.wrRegPort.addr    := executedSignals.wrRegAddr
  
    io.mem3Bypass.wrRegEn     := executedSignals.wrRegEn
    io.mem3Bypass.wrRegAddr   := executedSignals.wrRegAddr
    io.mem3Bypass.wrData      := wrData
    io.mem3Bypass.isLoad      := isLoad || executedSignals.rdCp0En
  }

}

class Mem extends Component {
  val io = new Bundle {
    val executedSignals  = Vec(in(new ExecutedSignals), 2)
    val dcacheMiss       = out Bool()
    val singleIssueStall = out Bool()
    val dcache           = Vec(master(new MemCachePort), 2)
    val wrRegPorts       = Vec(master(new WriteGeneralRegfilePort), 2)
    val tlbPort          = Vec(master(TranslateAddrReq()), 2)
    val mem0Bypass       = Vec(out(new BypassPort), 2)
    val mem1Bypass       = Vec(out(new BypassPort), 2)
    val mem2Bypass       = Vec(out(new BypassPort), 2)
    val mem3Bypass       = Vec(out(new BypassPort), 2)
    val rdCp0Addr        = out UInt(5 bits)
    val rdCp0Sel         = out UInt(3 bits)
    val rdCp0Data        = in UInt(32 bits)
    val wrCp0Port        = master(new Cp0WritePort)
    val exptReq          = master(ExptReq())
    val hwIntAvail       = out Bool()
    val hwIntTrig        = in Bool()

    val addrConflict     = in Bool()
  }

  val memArbiter = MemArbiter()
  // val memArbiterSignals0 = Reg(new ExecutedSignals) init(ExecutedSignals().nopExecutedSignals)
  // val memArbiterSignals1 = Reg(new ExecutedSignals) init(ExecutedSignals().nopExecutedSignals)
  val memArbiterSignals = Vec(RegInit(ExecutedSignals().nopExecutedSignals), 2)
  val mmuSignals        = Vec(RegInit(MmuSignals().nopMmuSignals), 2)
  memArbiter.io.inputsSignals := io.executedSignals
  memArbiter.io.stall := io.dcacheMiss
  memArbiter.io.hwIntTrig := io.hwIntTrig
  memArbiter.io.addrConflictEx := io.addrConflict
  io.singleIssueStall := memArbiter.io.singleIssueStall

  val singleMem0 = new SingleMem
  val singleMem1 = new SingleMem
  // singleMem0.io.executedSignals := memArbiter.io.outputSignals(0)
  // singleMem1.io.executedSignals := memArbiter.io.outputSignals(1)
  singleMem0.io.executedSignals := memArbiterSignals(0)
  singleMem1.io.executedSignals := memArbiterSignals(1)
  singleMem0.io.mmuSignals := mmuSignals(0)
  singleMem1.io.mmuSignals := mmuSignals(1)
  singleMem0.io.hwIntTrig := io.hwIntTrig
  singleMem1.io.hwIntTrig := io.hwIntTrig

  singleMem0.io.rdCp0Data := io.rdCp0Data
  singleMem1.io.rdCp0Data := io.rdCp0Data

  val dcacheMiss = singleMem0.io.cacheMiss || singleMem1.io.cacheMiss
  io.dcacheMiss := dcacheMiss
  singleMem0.io.stall := dcacheMiss
  singleMem1.io.stall := dcacheMiss
  io.wrRegPorts(0) := singleMem0.io.wrRegPort
  io.wrRegPorts(1) := singleMem1.io.wrRegPort
  io.dcache(0) <> singleMem0.io.dcache
  io.dcache(1) <> singleMem1.io.dcache

  // Mmu
  val mmu = for (i <- 0 until 2) yield Mmu()
  val isUnalinedLS = Vec(Bool(), 2)
  val vaddr = Vec(UInt(32 bits), 2)
  val vaddr_valid = Vec(Bool(), 2)
  val uncache   = Vec(Bool(), 2)
  val paddr     = Vec(UInt(32 bits), 2)
  val exptValid = Vec(Bool(), 2)
  val exptCode  = Vec(UInt(exptCodeWidth bits), 2)
  val reqValid  = Vec(Bool(), 2)  // used in CacheAccess

  val isLoad = Vec(Bool(), 2)
  val isStore = Vec(Bool(), 2)
  val isICacheInst = Vec(Bool(), 2)
  val isDCacheInst = Vec(Bool(), 2)
  val isCacheInst  = Vec(Bool(), 2)

  for (i <- 0 until 2) {
    isLoad(i) := memArbiter.io.outputSignals(i).rdMemEn && !io.hwIntTrig
    isStore(i) := memArbiter.io.outputSignals(i).wrMemEn && !io.hwIntTrig
    isICacheInst(i) := memArbiter.io.outputSignals(i).isICacheInst && !io.hwIntTrig
    isDCacheInst(i) := memArbiter.io.outputSignals(i).isDCacheInst && !io.hwIntTrig
    isCacheInst(i) := isICacheInst(i) || isDCacheInst(i)

    switch (memArbiter.io.outputSignals(i).uop) {
      is (uOpSwl, uOpSwr, uOpLwl, uOpLwr) {
        isUnalinedLS(i) := True
      }
      default { isUnalinedLS(i) := False }
    }
    vaddr(i) := memArbiter.io.outputSignals(i).memVAddr(31 downto 2) @@ Mux(
      isUnalinedLS(i), U(0, 2 bits), memArbiter.io.outputSignals(i).memVAddr(1 downto 0)
    )
    vaddr_valid(i) := isLoad(i) || isStore(i) || isDCacheInst(i)

    mmu(i).io.vaddr := vaddr(i)
    mmu(i).io.is_write := memArbiter.io.outputSignals(i).wrMemMask =/= 0
    mmu(i).io.tlbPort <> io.tlbPort(i)
    uncache(i) := mmu(i).io.uncache
    paddr(i) := mmu(i).io.paddr  // not the cacheAccess signal
    exptValid(i) := mmu(i).io.exptValid && vaddr_valid(i)
    exptCode(i) := mmu(i).io.exptCode
    if (i == 1) {
      when (mmu(0).io.exptValid && vaddr_valid(0)) {
        exptValid(i) := True
      }
    }
    reqValid(i) := vaddr_valid(i) && !mmu(i).io.exptValid
    if (i == 1) {
      when (mmu(0).io.exptValid && vaddr_valid(0)) {
        reqValid(i) := False
      }
    }

  }
  // Reg to Mem1
  for (i <- 0 until 2) {
    when (dcacheMiss) {
      memArbiterSignals(i) := memArbiterSignals(i)
      mmuSignals(i) := mmuSignals(i)
    } elsewhen (io.hwIntTrig/* || io.except.exptValid*/) {  // *** test point ***
      memArbiterSignals(i) := ExecutedSignals().nopExecutedSignals
      mmuSignals(i) := MmuSignals().nopMmuSignals
    } otherwise {
      memArbiterSignals(i) := memArbiter.io.outputSignals(i)
      mmuSignals(i).vaddr       := vaddr(i)
      mmuSignals(i).vaddr_valid := vaddr_valid(i)
      mmuSignals(i).uncache     := uncache(i)
      mmuSignals(i).paddr       := paddr(i)
      mmuSignals(i).exptValid   := exptValid(i)
      mmuSignals(i).exptCode    := exptCode(i)
      mmuSignals(i).reqValid    := reqValid(i)
    }
  }
  
  // Cp0
  when (memArbiterSignals(1).wrCp0En && !io.hwIntTrig) {
    io.wrCp0Port.wen  := True
    io.wrCp0Port.sel  := memArbiterSignals(1).cp0Sel
    io.wrCp0Port.addr := memArbiterSignals(1).cp0Addr
    io.wrCp0Port.data := memArbiterSignals(1).wrData
    io.wrCp0Port.pc   := memArbiterSignals(1).pc
  } otherwise {
    io.wrCp0Port.wen  := memArbiterSignals(0).wrCp0En
    io.wrCp0Port.sel  := memArbiterSignals(0).cp0Sel
    io.wrCp0Port.addr := memArbiterSignals(0).cp0Addr
    io.wrCp0Port.data := memArbiterSignals(0).wrData
    io.wrCp0Port.pc   := memArbiterSignals(0).pc
  }
  io.hwIntAvail := memArbiterSignals(0).pc(11 downto 0) =/= 0
  when (memArbiterSignals(0).rdCp0En) {
    io.rdCp0Addr := memArbiterSignals(0).cp0Addr
    io.rdCp0Sel  := memArbiterSignals(0).cp0Sel
  } otherwise {
    io.rdCp0Addr := memArbiterSignals(1).cp0Addr
    io.rdCp0Sel  := memArbiterSignals(1).cp0Sel
  }

  // Exception
  val exceptValid = Vec(Bool(), 2)
  exceptValid(0) := memArbiterSignals(0).except.exptValid || singleMem0.io.except.exptValid
  exceptValid(1) := memArbiterSignals(1).except.exptValid || singleMem1.io.except.exptValid
  val exceptInfo  = Vec(ExptInfo(), 2)
  exceptInfo(0)  := Mux(memArbiterSignals(0).except.exptValid, 
                        memArbiterSignals(0).except,
                        singleMem0.io.except)
  exceptInfo(1)  := Mux(memArbiterSignals(1).except.exptValid,
                        memArbiterSignals(1).except,
                        singleMem1.io.except)

  when ((exceptValid(0) && 
         memArbiterSignals(0).except.exptCode =/= EXC_ADEL_FI) ||
         !exceptValid(1)) {
    io.exptReq.exptInfo := exceptInfo(0)
    io.exptReq.exptPc   := memArbiterSignals(0).pc
    io.exptReq.memVAddr := memArbiterSignals(0).memVAddr
    io.exptReq.inBD     := False
  } otherwise {
    io.exptReq.exptInfo := exceptInfo(1)
    io.exptReq.memVAddr := memArbiterSignals(1).memVAddr
    when (memArbiterSignals(0).isBr) {
      io.exptReq.exptPc := memArbiterSignals(0).pc
      io.exptReq.inBD   := True
    } otherwise {
      io.exptReq.exptPc := memArbiterSignals(1).pc
      io.exptReq.inBD   := False
    }
  }

  when (dcacheMiss) {
    io.exptReq.exptInfo.exptValid := False
  }

  // Mem0Bypass
  for (i <- 0 until 2) {
    io.mem0Bypass(i).wrRegEn   := memArbiter.io.outputSignals(i).wrRegEn
    io.mem0Bypass(i).wrRegAddr := memArbiter.io.outputSignals(i).wrRegAddr
    io.mem0Bypass(i).wrData    := memArbiter.io.outputSignals(i).wrData
    io.mem0Bypass(i).isLoad    := isLoad(i) || memArbiter.io.outputSignals(i).rdCp0En
  }

  io.mem1Bypass(0) := singleMem0.io.mem1Bypass
  io.mem1Bypass(1) := singleMem1.io.mem1Bypass
  io.mem2Bypass(0) := singleMem0.io.mem2Bypass
  io.mem2Bypass(1) := singleMem1.io.mem2Bypass
  io.mem3Bypass(0) := singleMem0.io.mem3Bypass
  io.mem3Bypass(1) := singleMem1.io.mem3Bypass

}

case class ReadDataAlign() extends Component {
  val io = new Bundle {
    val raw_data = in UInt(32 bits)
    val size = in UInt(3 bits)
    val is_signed = in Bool()
    val addr10 = in UInt(2 bits)
    val original = in UInt(32 bits)
    val op = in(Uops())
    val data_o = out UInt(32 bits)
  }
  io.data_o := io.raw_data
  switch(io.size) {
    is(CReq.MSIZE1) {
      switch(io.addr10) {
        is(1) {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(15 downto 8), 32 bits)), U(0, 24 bits) @@ io.raw_data(15 downto 8))
        }
        is(2) {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(23 downto 16), 32 bits)), U(0, 24 bits) @@ io.raw_data(23 downto 16))
        }
        is(3) {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(31 downto 24), 32 bits)), U(0, 24 bits) @@ io.raw_data(31 downto 24))
        }
        default {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(7 downto 0), 32 bits)), U(0, 24 bits) @@ io.raw_data(7 downto 0))
        }
      }
    } // MSIZE1
    is(CReq.MSIZE2) {
      switch(io.addr10) {
        is(2) {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(31 downto 16), 32 bits)), U(0, 16 bits) @@ io.raw_data(31 downto 16))
        } 
        default {
          io.data_o := Mux(io.is_signed, U(S(io.raw_data(15 downto 0), 32 bits)), U(0, 16 bits) @@ io.raw_data(15 downto 0))
        }
      }
    } // MSIZE2
    default { // MSIZE4
      switch(io.op) {
        is(uOpLwl) {
          switch(io.addr10) {
            is(1) {
              io.data_o := io.raw_data(15 downto 0) @@ io.original(15 downto 0)
            }
            is(2) {
              io.data_o := io.raw_data(23 downto 0) @@ io.original(7 downto 0)
            }
            is(3) {
              // io.data_o := io.raw_data
            }
            default {
              io.data_o := io.raw_data(7 downto 0) @@ io.original(23 downto 0)
            }
          }
        } // lwl
        default { // is(uOpLwr) {
          switch(io.addr10) {
            is(1) {
              io.data_o := io.original(31 downto 24) @@ io.raw_data(31 downto 8)
            }
            is(2) {
              io.data_o := io.original(31 downto 16) @@ io.raw_data(31 downto 16)
            }
            is(3) {
              io.data_o := io.original(31 downto 8) @@ io.raw_data(31 downto 24)
            }
            default {
              // io.data_o := io.raw_data
            }
          }
        } // lwr, lw
      }
    }// MSIZE4
  }
}

object GenMemVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Mem)
  }
}