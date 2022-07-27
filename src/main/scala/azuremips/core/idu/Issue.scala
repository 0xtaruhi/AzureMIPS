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
  when (((io.inst1.op1RdGeRf && (io.inst1.op1Addr === io.inst0.wrRegAddr)) ||
         (io.inst1.op2RdGeRf && (io.inst1.op2Addr === io.inst0.wrRegAddr))) &&
          io.inst0.wrRegEn && io.inst0.wrRegAddr =/= 0) {
    io.singleIssue := True
  }
  when (io.inst0.wrRegAddr === io.inst1.wrRegAddr && io.inst0.wrRegEn && io.inst1.wrRegEn && 
        io.inst0.wrRegAddr =/= 0) {
    io.singleIssue := True
  }
  when (io.inst0.isPriv || (io.inst1.isPriv && !io.inst0.isBr)) {
    io.singleIssue := True
  }
  when (io.inst0.useHilo && io.inst1.useHilo) {
    io.singleIssue := True
  }
  when (io.inst0.multiCycle || (io.inst1.multiCycle && !io.inst0.isBr)) {
    io.singleIssue := True
  }
}

class Issue extends Component {
  val io = new Bundle {
    val stall       = in Bool()
    val decodeInst0 = in(new DecodedSignals)
    val decodeInst1 = in(new DecodedSignals)
    val issueInst0  = out(new DecodedSignals)
    val issueInst1  = out(new DecodedSignals)
    val prevStall   = out Bool()
  }

  val arbiter = new IssueArbiter()
  arbiter.io.inst0 := io.decodeInst0
  arbiter.io.inst1 := io.decodeInst1

  val nopDecodedSignals = DecodedSignals().nopDecodedSignals

  val inSingleIssue = Reg(Bool) init (False)
  when (!io.stall) {
    when (inSingleIssue) {
      inSingleIssue := False
    } elsewhen (arbiter.io.singleIssue) {
      inSingleIssue := True
    }
  }

  io.prevStall := False
  when (inSingleIssue) {
    io.issueInst0 := io.decodeInst1
    io.issueInst1 := nopDecodedSignals
  } elsewhen (arbiter.io.singleIssue) {
    io.issueInst0 := io.decodeInst0
    io.issueInst1 := nopDecodedSignals
    io.prevStall  := True
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