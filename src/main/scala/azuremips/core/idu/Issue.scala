package azuremips.core.idu

import spinal.core._
import spinal.lib._
import azuremips.core._

class IssueArbiter extends Component {
  val io = new Bundle {
    val inst0 = in(new DecodedSignals)
    val inst1 = in(new DecodedSignals)
    val singleIssue = out Bool()
  }
  io.singleIssue   := False
  when (io.inst1.op1RdGeRf && io.inst0.wrRegEn && io.inst0.op1Addr === io.inst1.wrRegAddr ||
        io.inst1.op2RdGeRf && io.inst0.wrRegEn && io.inst0.op2Addr === io.inst1.wrRegAddr) {
    io.singleIssue := True
  }
  when (io.inst0.isPriv || io.inst1.isPriv) {
    io.singleIssue := True
  }
}

class Issue extends Component {
  val io = new Bundle {
    val decodeInst0 = in(new DecodedSignals)
    val decodeInst1 = in(new DecodedSignals)
    val issueInst0  = out(new DecodedSignals)
    val issueInst1  = out(new DecodedSignals)
    val prevStall   = out Bool()
  }

  val arbiter = new IssueArbiter()
  arbiter.io.inst0 := io.decodeInst0
  arbiter.io.inst1 := io.decodeInst1

  val nopDecodedSignals = new DecodedSignals()
  nopDecodedSignals.pc        := 0x0
  nopDecodedSignals.op1Addr   := 0x0
  nopDecodedSignals.op1RdGeRf := False
  nopDecodedSignals.op2Addr   := 0x0
  nopDecodedSignals.op2RdGeRf := False
  nopDecodedSignals.wrRegAddr := 0x0
  nopDecodedSignals.wrRegEn   := False
  nopDecodedSignals.uop       := Uops.uOpSll
  nopDecodedSignals.useImm    := False
  nopDecodedSignals.imm       := 0x0
  nopDecodedSignals.isPriv    := False
  nopDecodedSignals.multiCycle:= False

  val inSingleIssue = Reg(Bool) init (False)

  io.prevStall := False
  when (inSingleIssue) {
    io.issueInst0 := io.decodeInst1
    io.issueInst1 := nopDecodedSignals
    inSingleIssue := False
  } elsewhen (arbiter.io.singleIssue) {
    io.issueInst0 := io.decodeInst0
    io.issueInst1 := nopDecodedSignals
    io.prevStall  := True
    inSingleIssue := True
  } otherwise {
    io.issueInst0 := io.decodeInst0
    io.issueInst1 := io.decodeInst1
  }
}

object GenIssueVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Issue)
  }
}