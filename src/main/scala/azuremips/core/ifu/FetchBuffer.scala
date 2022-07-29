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
    val multiCycleStall = in Bool()
    val full       = out Bool()
  }

  val buffer = Vec(Reg(InstWithPcInfo()), depth)
  // init
  for (entry <- buffer) {
    entry.valid   init (False)
    entry.payload init (0)
    entry.pc      init (0)
    entry.isBr    init (False)
    entry.isNop   init (False)
  }
  val tailPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val headPtr = Reg(UInt(log2Up(depth) bits)) init 0
  val diffCycle = Reg(Bool()) init False
  val occupiedNum = Mux(diffCycle, 16 - (headPtr - tailPtr), tailPtr - headPtr)
  val availNum    = 16 - occupiedNum
  
  val buffer1Skip = buffer(headPtr).isNop
  val buffer2Skip = buffer(headPtr + 1).isNop & !buffer(headPtr).isBr // including this situation: buffer(headPtr + 1).isNop == Ture but invalid (occupiedNum == 1)

  io.full := (availNum < 8)
  val stall_push = (availNum < 4)

  val validInstCnt = io.pushInsts.map(_.valid.asUInt.resize(3)).reduce(_ + _)

  when (buffer1Skip) {
    for (i <- 0 until 2) {
      when (buffer(headPtr + i + 1).valid && !io.flush) {
        io.popInsts(i) := buffer(headPtr + i + 1).payload
        io.popPc(i)    := buffer(headPtr + i + 1).pc
      } otherwise {
        io.popInsts(i) := U(0)
        io.popPc(i)    := U(0)
      }
    }
  } elsewhen(buffer2Skip) {
    when (buffer(headPtr).valid && !io.flush) {
      io.popInsts(0) := buffer(headPtr).payload
      io.popPc(0)    := buffer(headPtr).pc
    } otherwise {
      io.popInsts(0) := U(0)
      io.popPc(0)    := U(0)
    }
    when (buffer(headPtr + 2).valid && !io.flush) {
      io.popInsts(1) := buffer(headPtr + 2).payload
      io.popPc(1)    := buffer(headPtr + 2).pc
    } otherwise {
      io.popInsts(1) := U(0)
      io.popPc(1)    := U(0)
    }
  } otherwise {
    for (i <- 0 until 2) {
      when (buffer(headPtr + i).valid && !io.flush) {
        io.popInsts(i) := buffer(headPtr + i).payload
        io.popPc(i)    := buffer(headPtr + i).pc
      } otherwise {
        io.popInsts(i) := U(0)
        io.popPc(i)    := U(0)
      }
    }
  }
  when (!io.flush && !io.popStall) {
    when (buffer1Skip || buffer2Skip) {
      buffer(headPtr).valid := False
      buffer(headPtr + 1).valid := False
      when (!buffer(headPtr + 2).isBr) {
        buffer(headPtr + 2).valid := False    
      }
    } otherwise {
      buffer(headPtr).valid := False
      when (!buffer(headPtr + 1).isBr) {
        buffer(headPtr + 1).valid := False    
      }
    }
  }

  val nextHeadPtr = UInt(log2Up(depth) bits)
  val nextTailPtr = UInt(log2Up(depth) bits)

  when (!stall_push) { //when (!io.stall) {
    nextTailPtr := tailPtr + validInstCnt
    for (i <- 0 until 4) {
      when (io.pushInsts(i).valid) {
        buffer(tailPtr + i) := io.pushInsts(i)
      }
    }
  } otherwise { nextTailPtr := tailPtr }

  when (!io.popStall) {
    when (buffer1Skip || buffer2Skip) {
      when (occupiedNum < 3) {
        nextHeadPtr := tailPtr
      } elsewhen (buffer(headPtr + 2).isBr) {
        nextHeadPtr := headPtr + 2
      } otherwise {
        nextHeadPtr := headPtr + 3
      }
    } otherwise {
      when (occupiedNum < 2) {
        nextHeadPtr := tailPtr
      } elsewhen (buffer(headPtr + 1).isBr) {
        nextHeadPtr := headPtr + 1
      } otherwise {
        nextHeadPtr := headPtr + 2
      }
    }
  } otherwise { nextHeadPtr := headPtr }
  
  headPtr := nextHeadPtr
  tailPtr := nextTailPtr
  diffCycle := diffCycle ^ (nextTailPtr < tailPtr) ^ (nextHeadPtr < headPtr)

  when (io.flush && !io.multiCycleStall) { // multicycle inst in delay slot, we need stall
    buffer.map(_.valid := False)
    buffer.map(_.payload := 0)
    buffer.map(_.pc := 0)
    headPtr := 0
    tailPtr := 0
    diffCycle := False
  }

  when (buffer1Skip || buffer2Skip) {
    when (buffer(headPtr + 2).isBr) {
      io.popInsts(1) := 0
    }
  } otherwise {
    when (buffer(headPtr + 1).isBr) {
      io.popInsts(1) := 0
    }
  }
}

object GenFetchBufferVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new FetchBuffer(16))
  }
}