package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import azuremips.core.exu.ExecutedSignals

case class MemArbiter() extends Component {
  val io = new Bundle {
    val stall         = in Bool()
    val hwIntTrig     = in Bool()
    val inputsSignals = in Vec(ExecutedSignals(), 2)
    val outputSignals = out Vec(ExecutedSignals(), 2)
    val singleIssueStall = out Bool()
  }

  def getPAddr(vaddr: UInt): UInt = {
    val paddr = UInt(32 bits)
    paddr := vaddr
    when(vaddr(31) === True && vaddr(30) === False) {
      paddr := U"000" @@ vaddr(28 downto 0)
    }
    paddr
  }

  def isUncacheAddr(vaddr : UInt) : Bool = {
    vaddr(31 downto 29) === U"101"
  }

  def getPAddrConflict(paddr1: UInt, paddr2: UInt): Bool = {
    paddr1(11 downto 2) === paddr2(11 downto 2)
  }

  val addrConflict = {
    val vaddr   = io.inputsSignals.map(_.memVAddr)
    val bothMem = io.inputsSignals.map(sig => sig.wrMemEn || sig.rdMemEn).reduce(_ && _)
    val paddr   = vaddr.map(getPAddr(_))
    val paddrConflict = getPAddrConflict(vaddr(0), vaddr(1)) // use vaddr for it's <= 4KB range
    val bothRd  = io.inputsSignals.map(_.rdMemEn).reduce(_ && _)

    bothMem &&
    ((paddrConflict && !bothRd) ||
    (vaddr.map(isUncacheAddr).reduce(_ ^ _)))
  }

  io.outputSignals := io.inputsSignals

  val fsm = new StateMachine {
    val issueUpper : State = new State with EntryPoint {
      whenIsActive {
        when (addrConflict && !io.stall) {
          goto(issueLower)
        }
        when (addrConflict) {
          io.outputSignals(0) := io.inputsSignals(0)
          io.outputSignals(1) := ExecutedSignals().nopExecutedSignals
        }
      }
    }

    val issueLower : State = new State {
      whenIsActive {
        when (addrConflict && !io.stall || io.hwIntTrig) {
          goto(issueUpper)
        }
        when (addrConflict) {
          io.outputSignals(0) := ExecutedSignals().nopExecutedSignals
          io.outputSignals(1) := io.inputsSignals(1)
        }
      }
    }
  }

  io.singleIssueStall := fsm.isActive(fsm.issueUpper) && addrConflict
}