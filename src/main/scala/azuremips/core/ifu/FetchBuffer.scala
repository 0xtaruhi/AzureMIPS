package azuremips.core.ifu

import spinal.core._
import spinal.lib._

class FetchBuffer(depth: Int = 16) extends Component {
  val io = new Bundle {
    val pushInsts  = Vec(in(InstWithPcInfo()), 4)
    val popInsts   = Vec(out(UInt(32 bits)), 2)
    val popPc      = Vec(out(UInt(32 bits)), 2)
    val flush      = in Bool()
    val stall      = in Bool()
    val popStall   = in Bool()
    val full       = out Bool()
    val stall_push = out Bool()
  }

  val buffer = Reg(Vec(InstWithPcInfo(), depth))
  val tailPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val headPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val diffCycle = Reg(Bool()) init False
  val occupiedNum = Mux(diffCycle, 16 - (headPtr - tailPtr), tailPtr - headPtr)
  val availNum    = 16 - occupiedNum

  io.full := (availNum < 8)
  io.stall_push := (availNum < 4)

  val validInstCnt = io.pushInsts.map(_.valid.asUInt.resize(3)).reduce(_ + _)


  for (i <- 0 until 2) {
    when (buffer(headPtr + i).valid && !io.flush) {
      io.popInsts(i) := buffer(headPtr + i).payload
      io.popPc(i)    := buffer(headPtr + i).pc
    } otherwise {
      io.popInsts(i) := U(0)
      io.popPc(i)    := U(0)
    }
  }
  when (!io.flush && !io.popStall) {
    buffer(headPtr).valid := False
    when (!buffer(headPtr + 1).isBr) {
      buffer(headPtr + 1).valid := False    
    }  
  }

  val nextHeadPtr = UInt(log2Up(depth) bits)
  val nextTailPtr = UInt(log2Up(depth) bits)

  when (!io.stall_push) { //when (!io.stall) {
    nextTailPtr := tailPtr + validInstCnt
    for (i <- 0 until 4) {
      when (io.pushInsts(i).valid) {
        buffer(tailPtr + i) := io.pushInsts(i)
      }
    }
  } otherwise { nextTailPtr := tailPtr }

  when (!io.popStall) {
    when (occupiedNum < 2) {
      nextHeadPtr := tailPtr
    } elsewhen (buffer(headPtr + 1).isBr) {
      nextHeadPtr := headPtr + 1
    } otherwise {
      nextHeadPtr := headPtr + 2
    }
  } otherwise { nextHeadPtr := headPtr }
  
  headPtr := nextHeadPtr
  tailPtr := nextTailPtr
  diffCycle := diffCycle ^ (nextTailPtr < tailPtr) ^ (nextHeadPtr < headPtr)

  when (io.flush) {
    buffer.map(_.valid := False)
    buffer.map(_.payload := 0)
    buffer.map(_.pc := 0)
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