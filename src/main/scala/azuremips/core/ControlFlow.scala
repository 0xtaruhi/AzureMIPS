package azuremips.core

import spinal.core._
import spinal.lib._

class ControlFlow extends Component {
  val io = new Bundle {
    val inputs = new Bundle {
      val fetchBufferFull   = in Bool()
      val singleIssue       = in Bool()
      val branchPredictMiss = in Bool()
      val dcacheMiss        = in Bool()
      val memSingleIssue    = in Bool()
      val loadRawStall      = in Bool()
      val multiCycleStall   = in Bool()
      val cp0Redirect       = in Bool()
    } 

    val outputs = new Bundle {
      val fetchStall      = out Bool()
      val fetchFlush      = out Bool()
      val fetchBufferPopStall = out Bool()
      val decodeStall     = out Bool()
      val readrfStall     = out Bool()
      val executeStall    = out Bool()
      val memStall        = out Bool()
      val multiCycleFlush = out Bool()
    } 
  }
  
  io.outputs.fetchStall   := False
  io.outputs.fetchBufferPopStall := False
  io.outputs.decodeStall  := False
  io.outputs.readrfStall  := False
  io.outputs.executeStall := False
  io.outputs.memStall     := False
  io.outputs.fetchFlush   := False
  io.outputs.multiCycleFlush   := False

  when (io.inputs.fetchBufferFull) {
    io.outputs.fetchStall  := True
  }

  when (io.inputs.singleIssue) {
    io.outputs.fetchBufferPopStall := True
    io.outputs.decodeStall := True
  }

  when (io.inputs.dcacheMiss) {
    io.outputs.fetchBufferPopStall := True
    io.outputs.decodeStall  := True
    io.outputs.readrfStall  := True
    io.outputs.executeStall := True
    io.outputs.memStall     := True
  }

  when (io.inputs.memSingleIssue) {
    io.outputs.fetchBufferPopStall := True
    io.outputs.decodeStall  := True
    io.outputs.readrfStall  := True
    io.outputs.executeStall := True
  }

  when (io.inputs.multiCycleStall) {
    io.outputs.fetchBufferPopStall := True
    io.outputs.decodeStall  := True
    io.outputs.readrfStall  := True
    // io.outputs.executeStall := True
  }

  when (io.inputs.branchPredictMiss) {
    io.outputs.fetchFlush   := True
    io.outputs.fetchBufferPopStall := False
    io.outputs.decodeStall  := False
    io.outputs.multiCycleFlush := True
  }

  when (io.inputs.loadRawStall) {
    io.outputs.fetchBufferPopStall := True
    io.outputs.decodeStall  := True
    io.outputs.readrfStall  := True
  }

  when (io.inputs.cp0Redirect) {
    io.outputs.fetchFlush   := True
    io.outputs.multiCycleFlush := True
  } 
}

object GenControlFlowVerilog extends App {
  SpinalVerilog(new ControlFlow)
}