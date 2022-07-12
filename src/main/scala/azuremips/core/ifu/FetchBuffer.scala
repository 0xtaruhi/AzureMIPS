package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class FetchBuffer(
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val instPack     = in(Vec(Flow(UInt(32 bits)), config.ifConfig.instFetchNum))
    val full         = out Bool()
    val insts2Decode = out(Vec(Flow(UInt(32 bits)), config.idConfig.decodeWayNum))
  }

  val fetchBufferDepth = config.ifConfig.fetchBufferDepth
  val decodeWayNum     = config.idConfig.decodeWayNum
  val fetchBufferReg = Vec(Reg(UInt(32 bits)) init(0), fetchBufferDepth)
  val fetchBufferAddrWidth = log2Up(fetchBufferDepth)

  val headPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val tailPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val diffCycle = Reg(Bool()) init(False)

  val availNum = tailPtr - headPtr + Mux(diffCycle, fetchBufferDepth, 0)

  io.full := (availNum < config.ifConfig.instFetchNum)

  val validInstCnt = UInt(log2Up(config.ifConfig.instFetchNum + 1) bits)
  validInstCnt := io.instPack.map(_.valid.asUInt.resize(validInstCnt.getWidth)).reduce(_ + _)

  val update = new Area {
    val nextTailPtr = UInt(log2Up(fetchBufferDepth) bits)
    val nextHeadPtr = UInt(log2Up(fetchBufferDepth) bits)

    if (isPow2(fetchBufferDepth)) {
      // nextHeadPtr := headPtr + Mux(availNum < decodeWayNum, availNum, decodeWayNum)
      when (availNum < decodeWayNum) {
        nextHeadPtr := tailPtr
      } otherwise { 
        nextHeadPtr := headPtr + decodeWayNum 
      }

      when (!io.full) {
        nextTailPtr := tailPtr + validInstCnt
      }
      diffCycle := diffCycle ^ (nextHeadPtr.msb ^ headPtr.msb) ^ (nextTailPtr.msb ^ tailPtr.msb)
    } else {
      // TODO: implement this
    }
    headPtr := nextHeadPtr
    tailPtr := nextTailPtr
    // when (!io.full) {
    //   if (isPow2(fetchBufferDepth)) {
    //     nextTailPtr := tailPtr + validInstCnt
    //     tailPtr := nextTailPtr
    //     diffCycle := diffCycle ^ (nextTailPtr.msb ^ tailPtr.msb) ^
    //                   (nextHeadPtr.msb ^ headPtr.msb)
    //   } else {
    //     nextTailPtr := Mux(tailPtr + validInstCnt < U(fetchBufferDepth),
    //       tailPtr + validInstCnt,
    //       tailPtr + validInstCnt - U(fetchBufferDepth))
    //     tailPtr := nextTailPtr
    //     diffCycle := diffCycle ^ (nextTailPtr < tailPtr) ^ (nextHeadPtr < headPtr)
    //   }
    // }
  }

  for (i <- 0 until decodeWayNum) {
    io.insts2Decode(i).payload := fetchBufferReg(headPtr + i)
    when (i >= availNum) {
      io.insts2Decode(i).valid := False
    }
  }

}

object genFetchBufferVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(FetchBuffer())
  }
}