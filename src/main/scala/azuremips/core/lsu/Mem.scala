package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._
import azuremips.core.cache.{DReq, DResp, CReq}
import azuremips.core.Uops._
import azuremips.core.exu.ExecutedSignals
import azuremips.core.reg.WriteGeneralRegfilePort
import azuremips.core.cp0.{Cp0ReadPort, Cp0WritePort, ExptInfo, ExptReq}

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
    val vaddr_valid = Bool()
    val vaddr       = UInt(32 bits)
    val strobe      = UInt(4 bits)
    val size        = UInt(3 bits)
    val data        = UInt(32 bits)
  }
  val rsp = new Bundle {
    val hit         = Bool()
    val data        = UInt(32 bits)
  }
  val exptValid     = Bool()
  val exptCode      = UInt(exptCodeWidth bits)
  override def asMaster(): Unit = {
    out(req)
    in(rsp, exptValid, exptCode)
  }
}

class SingleMem extends Component {
  val io = new Bundle {
    val executedSignals = in(new ExecutedSignals)
    val stall           = in Bool()
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
    
    io.dcache.req.vaddr       := io.executedSignals.memVAddr
    io.dcache.req.vaddr_valid := (io.executedSignals.wrMemEn || io.executedSignals.rdMemEn) && !io.hwIntTrig
    io.dcache.req.data        := io.executedSignals.wrData
    io.dcache.req.strobe      := io.executedSignals.wrMemMask
    io.dcache.req.size        := io.executedSignals.memSize

    io.mem1Bypass.wrRegEn     := io.executedSignals.wrRegEn && !io.hwIntTrig
    io.mem1Bypass.wrRegAddr   := io.executedSignals.wrRegAddr
    io.mem1Bypass.wrData      := io.executedSignals.wrData
    io.mem1Bypass.isLoad      := isLoad || io.executedSignals.rdCp0En

    val memSize = io.executedSignals.memSize
    val signExt = io.executedSignals.signExt

    io.except.exptValid := (isLoad || isStore) && io.dcache.exptValid
    io.except.exptCode  := io.dcache.exptCode
    io.except.eret      := False

  }

  val stage2 = new Area {
    val isLoad  = RegInit(False)
    val isStore = RegInit(False)
    val executedSignals = RegInit(ExecutedSignals().nopExecutedSignals)

    when (io.stall) {
      isLoad := isLoad
      isStore := isStore
      executedSignals := executedSignals
    }.elsewhen (io.hwIntTrig || io.except.exptValid) {
      isLoad  := False
      isStore := False
      executedSignals := ExecutedSignals().nopExecutedSignals
    }.otherwise {
      isLoad  := stage1.isLoad
      isStore := stage1.isStore
      executedSignals := io.executedSignals
      when (io.executedSignals.rdCp0En) {
        executedSignals.wrData := io.rdCp0Data
      }
    }

    when ((isLoad || isStore) && !io.dcache.rsp.hit) {
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

    val wrData = UInt(32 bits)
    when (executedSignals.rdMemEn) {
      wrData := readDataAlignInst.io.data_o
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
    val mem1Bypass       = Vec(out(new BypassPort), 2)
    val mem2Bypass       = Vec(out(new BypassPort), 2)
    val mem3Bypass       = Vec(out(new BypassPort), 2)
    val rdCp0Data        = in UInt(32 bits)
    val wrCp0Port        = master(new Cp0WritePort)
    val exptReq          = master(ExptReq())
    val hwIntAvail       = out Bool()
    val hwIntTrig        = in Bool()

    val addrConflict     = in Bool()
  }

  val memArbiter = MemArbiter()
  memArbiter.io.inputsSignals := io.executedSignals
  memArbiter.io.stall := io.dcacheMiss
  memArbiter.io.hwIntTrig := io.hwIntTrig
  memArbiter.io.addrConflictEx := io.addrConflict
  io.singleIssueStall := memArbiter.io.singleIssueStall

  val singleMem0 = new SingleMem
  val singleMem1 = new SingleMem
  singleMem0.io.executedSignals := memArbiter.io.outputSignals(0)
  singleMem1.io.executedSignals := memArbiter.io.outputSignals(1)
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
  // Cp0
  when (memArbiter.io.outputSignals(1).wrCp0En && !io.hwIntTrig) {
    io.wrCp0Port.wen  := True
    io.wrCp0Port.sel  := memArbiter.io.outputSignals(1).cp0Sel
    io.wrCp0Port.addr := memArbiter.io.outputSignals(1).cp0Addr
    io.wrCp0Port.data := memArbiter.io.outputSignals(1).wrData
    io.wrCp0Port.pc   := memArbiter.io.outputSignals(1).pc
  } otherwise {
    io.wrCp0Port.wen  := memArbiter.io.outputSignals(0).wrCp0En
    io.wrCp0Port.sel  := memArbiter.io.outputSignals(0).cp0Sel
    io.wrCp0Port.addr := memArbiter.io.outputSignals(0).cp0Addr
    io.wrCp0Port.data := memArbiter.io.outputSignals(0).wrData
    io.wrCp0Port.pc   := memArbiter.io.outputSignals(0).pc
  }
  io.hwIntAvail := memArbiter.io.outputSignals(0).pc(11 downto 0) =/= 0

  // Exception
  val exceptValid = Vec(Bool(), 2)
  exceptValid(0) := memArbiter.io.outputSignals(0).except.exptValid || singleMem0.io.except.exptValid
  exceptValid(1) := memArbiter.io.outputSignals(1).except.exptValid || singleMem1.io.except.exptValid
  val exceptInfo  = Vec(ExptInfo(), 2)
  exceptInfo(0)  := Mux(memArbiter.io.outputSignals(0).except.exptValid, 
                        memArbiter.io.outputSignals(0).except,
                        singleMem0.io.except)
  exceptInfo(1)  := Mux(memArbiter.io.outputSignals(1).except.exptValid,
                        memArbiter.io.outputSignals(1).except,
                        singleMem1.io.except)

  when ((exceptValid(0) && 
         memArbiter.io.outputSignals(0).except.exptCode =/= EXC_ADEL_FI) ||
         !exceptValid(1)) {
    io.exptReq.exptInfo := exceptInfo(0)
    io.exptReq.exptPc   := memArbiter.io.outputSignals(0).pc
    io.exptReq.memVAddr := memArbiter.io.outputSignals(0).memVAddr
    io.exptReq.inBD     := False
  } otherwise {
    io.exptReq.exptInfo := exceptInfo(1)
    io.exptReq.memVAddr := memArbiter.io.outputSignals(1).memVAddr
    when (memArbiter.io.outputSignals(0).isBr) {
      io.exptReq.exptPc := memArbiter.io.outputSignals(0).pc
      io.exptReq.inBD   := True
    } otherwise {
      io.exptReq.exptPc := memArbiter.io.outputSignals(1).pc
      io.exptReq.inBD   := False
    }
  }

  when (dcacheMiss) {
    io.exptReq.exptInfo.exptValid := False
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
    default {}// MSIZE4
  }
}

object GenMemVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Mem)
  }
}