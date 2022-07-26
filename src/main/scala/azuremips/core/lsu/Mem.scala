package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import azuremips.core._
import azuremips.core.cache.{DReq, DResp, CReq}
import azuremips.core.Uops._
import azuremips.core.exu.ExecutedSignals
import azuremips.core.reg.WriteGeneralRegfilePort

class DCachePort extends Bundle with IMasterSlave {
  val req = new DReq()
  val rsp = new DResp()

  override def asMaster(): Unit = {
    out(req)
    in(rsp)
  }
}

class SingleMem extends Component {
  val io = new Bundle {
    val executedSignals = in(new ExecutedSignals)
    val paddr           = in(UInt(32 bits))
    val stall           = in Bool()
    val dcache          = master(new DCachePort)
    val cacheMiss       = out Bool()
    val mem1Bypass      = out(new BypassPort)
    val mem2Bypass      = out(new BypassPort)
    val mem3Bypass      = out(new BypassPort)
    val wrRegPort       = master(new WriteGeneralRegfilePort)
  }

  val stage1 = new Area {
    val isLoad  = io.executedSignals.rdMemEn
    val isStore = io.executedSignals.wrMemEn
    io.dcache.req.vaddr       := io.executedSignals.memVAddr
    io.dcache.req.vaddr_valid := io.executedSignals.wrMemEn || io.executedSignals.rdMemEn
    io.dcache.req.paddr       := io.paddr
    io.dcache.req.paddr_valid := io.executedSignals.wrMemEn || io.executedSignals.rdMemEn
    io.dcache.req.data        := io.executedSignals.wrData
    io.dcache.req.strobe      := io.executedSignals.wrMemMask
    io.dcache.req.size        := CReq.MSIZE4

    io.mem1Bypass.wrRegEn     := io.executedSignals.wrRegEn
    io.mem1Bypass.wrRegAddr   := io.executedSignals.wrRegAddr
    io.mem1Bypass.wrData      := io.executedSignals.wrData
    io.mem1Bypass.isLoad      := isLoad

    val signExt   = io.executedSignals.signExt
    val memSize   = io.executedSignals.memSize
  }

  val stage2 = new Area {
    val isLoad  = RegNextWhen(stage1.isLoad,  !io.stall)
    val isStore = RegNextWhen(stage1.isStore, !io.stall)
    val executedSignals = RegNextWhen(io.executedSignals, !io.stall)

    when ((isLoad || isStore) && !io.dcache.rsp.hit) {
      io.cacheMiss := True
    } otherwise { io.cacheMiss := False }

    io.mem2Bypass.wrRegEn     := executedSignals.wrRegEn
    io.mem2Bypass.wrRegAddr   := executedSignals.wrRegAddr
    io.mem2Bypass.wrData      := executedSignals.wrData
    io.mem2Bypass.isLoad      := isLoad

    val signExt = RegNextWhen(stage1.signExt, !io.stall)
    val memSize = RegNextWhen(stage1.memSize, !io.stall)
  }

  val stage3 = new Area {
    val executedSignals = RegNextWhen(stage2.executedSignals, !io.stall)
    val isLoad  = RegNextWhen(stage2.isLoad,  !io.stall)
    val isStore = RegNextWhen(stage2.isStore, !io.stall)

    val signExt = RegNextWhen(stage2.signExt, !io.stall)
    val memSize = RegNextWhen(stage2.memSize, !io.stall)

    io.wrRegPort.wrEn    := executedSignals.wrRegEn && !io.stall
    io.wrRegPort.pc      := executedSignals.pc
    val dcacheRspData    = io.dcache.rsp.data
    // align dcacheRspData
    val readDataAlignInst = new ReadDataAlign()
    readDataAlignInst.io.size := memSize
    readDataAlignInst.io.is_signed := signExt
    readDataAlignInst.io.addr10 := executedSignals.memVAddr(1 downto 0)
    readDataAlignInst.io.raw_data := dcacheRspData

    io.wrRegPort.data    := Mux(executedSignals.rdMemEn, readDataAlignInst.io.data_o, executedSignals.wrData)
    io.wrRegPort.addr    := executedSignals.wrRegAddr
  
    io.mem3Bypass.wrRegEn     := executedSignals.wrRegEn
    io.mem3Bypass.wrRegAddr   := executedSignals.wrRegAddr
    io.mem3Bypass.wrData      := Mux(executedSignals.rdMemEn, readDataAlignInst.io.data_o, executedSignals.wrData)
    io.mem3Bypass.isLoad      := isLoad

  }

}

class Mem extends Component {
  val io = new Bundle {
    val executedSignals  = Vec(in(new ExecutedSignals), 2)
    val dcacheMiss       = out Bool()
    val singleIssueStall = out Bool()
    val dcache           = Vec(master(new DCachePort), 2)
    val wrRegPorts       = Vec(master(new WriteGeneralRegfilePort), 2)
    val mem1Bypass       = Vec(out(new BypassPort), 2)
    val mem2Bypass       = Vec(out(new BypassPort), 2)
    val mem3Bypass       = Vec(out(new BypassPort), 2)
  }

  def getPAddr(vaddr: UInt): UInt = {
    val paddr = UInt(32 bits)
    paddr := vaddr
    when(vaddr(31) === True && vaddr(30) === False) {
      paddr := U"000" @@ vaddr(28 downto 0)
    }
    paddr
  }
  // single issue when addrConflict
  val paddr0 = getPAddr(io.executedSignals(0).memVAddr)
  val paddr1 = getPAddr(io.executedSignals(1).memVAddr)
  val addrConflict = (paddr0(31 downto 2) === paddr1(31 downto 2)) && 
                      io.executedSignals.map(sig => sig.wrMemEn || sig.rdMemEn).reduce(_ && _) &&
                      !io.executedSignals.map(_.rdMemEn).reduce(_ && _)
  val singleMemSignal0 = new ExecutedSignals
  val singleMemSignal1 = new ExecutedSignals
  singleMemSignal0 := io.executedSignals(0)
  singleMemSignal1 := io.executedSignals(1)

  val fsm = new StateMachine {
    val issueUpper : State = new State with EntryPoint {
      whenIsActive {
        when (addrConflict && !io.dcacheMiss) {
          goto(issueLower)
        }
        when (addrConflict) {
          singleMemSignal0 := io.executedSignals(0)
          singleMemSignal1 := ExecutedSignals().nopExecutedSignals
        }
      }
    }
    val issueLower : State = new State {
      whenIsActive {
        when (addrConflict && !io.dcacheMiss) {
          goto(issueUpper)
        }
        when (addrConflict) {
          singleMemSignal0 := ExecutedSignals().nopExecutedSignals
          singleMemSignal1 := io.executedSignals(1)
        }
      }
    }
  }

  val singleMem0 = new SingleMem
  val singleMem1 = new SingleMem
  singleMem0.io.executedSignals := singleMemSignal0
  singleMem1.io.executedSignals := singleMemSignal1
  singleMem0.io.paddr := paddr0
  singleMem1.io.paddr := paddr1

  val singleIssueStall = fsm.isActive(fsm.issueUpper) && addrConflict
  io.singleIssueStall := singleIssueStall
  val dcacheMiss = singleMem0.io.cacheMiss || singleMem1.io.cacheMiss
  io.dcacheMiss := dcacheMiss
  singleMem0.io.stall := dcacheMiss
  singleMem1.io.stall := dcacheMiss
  io.wrRegPorts(0) := singleMem0.io.wrRegPort
  io.wrRegPorts(1) := singleMem1.io.wrRegPort
  io.dcache(0) <> singleMem0.io.dcache
  io.dcache(1) <> singleMem1.io.dcache

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