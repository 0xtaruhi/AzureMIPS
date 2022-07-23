package azuremips.core.ifu

import spinal.core._
import spinal.lib._

class FetchBuffer(depth: Int = 16) extends Component {
  val io = new Bundle {
    val pushInsts = Vec(in(InstWithPcInfo()), 4)
    val popInsts  = Vec(out(UInt(32 bits)), 2)
    val popPc     = Vec(out(UInt(32 bits)), 2)
    val flush     = in Bool()
    val stall     = in Bool()
    val full      = out Bool()
  }

  val buffer = Reg(Vec(InstWithPcInfo(), depth))
  val tailPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val headPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val diffCycle = Reg(Bool()) init False
  val occupiedNum = Mux(diffCycle, 16 - (headPtr - tailPtr), tailPtr - headPtr)
  val availNum    = 16 - occupiedNum

  io.full := (availNum < 4)

  val validInstCnt = io.pushInsts.map(_.valid.asUInt.resize(3)).reduce(_ + _)

  for (i <- 0 until 2) {
    when (!io.flush && !io.stall) {
      io.popInsts(i) := Mux(buffer(headPtr + i).valid, buffer(headPtr + i).payload, U(0))
      io.popPc(i) := buffer(headPtr + i).pc
      buffer(headPtr + i).valid := False
    } otherwise {
      io.popInsts(i) := 0
      io.popPc(i) := 0
    }
  }

  when (!io.stall) {
    val nextTailPtr = UInt(log2Up(depth) bits)
    val nextHeadPtr = UInt(log2Up(depth) bits)

    nextTailPtr := tailPtr + validInstCnt
    diffCycle := diffCycle ^ (nextTailPtr < tailPtr) ^ (nextHeadPtr < headPtr)

    for (i <- 0 until 4) {
      when (io.pushInsts(i).valid) {
        buffer(tailPtr + i) := io.pushInsts(i)
      }
    }

    when (occupiedNum < 2) {
      nextHeadPtr := tailPtr
    } elsewhen (buffer(headPtr + 1).isBr) {
      nextHeadPtr := headPtr + 1
    } otherwise {
      nextHeadPtr := headPtr + 2
    }
    headPtr := nextHeadPtr
    tailPtr := nextTailPtr
  }

  when (io.flush) {
    buffer.map(_.valid := False)
    headPtr := 0
    tailPtr := 0
    diffCycle := False
  }

  when (buffer(headPtr + 1).isBr) {
    io.popInsts(1) := 0
  }
}

object GenFetchBufferVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new FetchBuffer(16))
  }
}