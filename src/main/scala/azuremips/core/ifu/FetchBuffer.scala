package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class FetchBuffer(
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    // val instsPack    = in(Vec(Flow(UInt(32 bits)), config.ifConfig.instFetchNum))
    val instsPack    = in(Vec(InstWithPcInfo(config), config.ifConfig.instFetchNum))
    val full         = out Bool()
    // val insts2Decode = out(Vec(Flow(UInt(32 bits)), config.idConfig.decodeWayNum))
    val insts2Decode = out(Vec(InstWithPcInfo(config), config.idConfig.decodeWayNum))
  }

  val fetchBufferDepth = config.ifConfig.fetchBufferDepth
  val decodeWayNum     = config.idConfig.decodeWayNum
  // val fetchBufferReg = Vec(Reg(UInt(32 bits)) init(0), fetchBufferDepth)
  val fetchBufferReg = Vec(Reg(InstWithPcInfo(config)), fetchBufferDepth)
  val fetchBufferAddrWidth = log2Up(fetchBufferDepth)

  val headPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val tailPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val diffCycle = Reg(Bool()) init(False)

  // val occupiedNum = tailPtr - headPtr + Mux(diffCycle, fetchBufferDepth, 0)
  val occupiedNum = Mux(diffCycle, fetchBufferDepth - (headPtr - tailPtr), tailPtr - headPtr)
  val availNum = fetchBufferDepth - occupiedNum

  io.full := (availNum < config.ifConfig.instFetchNum)

  val validInstCnt = UInt(log2Up(config.ifConfig.instFetchNum + 1) bits)
  validInstCnt := io.instsPack.map(_.valid.asUInt.resize(validInstCnt.getWidth)).reduce(_ + _)

  val update = new Area {
    val nextTailPtr = UInt(log2Up(fetchBufferDepth) bits)
    val nextHeadPtr = UInt(log2Up(fetchBufferDepth) bits)

    if (isPow2(fetchBufferDepth)) {
      // nextHeadPtr := headPtr + Mux(availNum < decodeWayNum, availNum, decodeWayNum)
      when (occupiedNum < decodeWayNum) {
        nextHeadPtr := tailPtr
      } otherwise { 
        nextHeadPtr := headPtr + decodeWayNum 
      }

      when (!io.full) {
        nextTailPtr := tailPtr + validInstCnt
      } otherwise { nextTailPtr := tailPtr }
      // diffCycle := diffCycle ^ (nextHeadPtr.msb ^ headPtr.msb) ^ (nextTailPtr.msb ^ tailPtr.msb)
      diffCycle := diffCycle ^ (nextHeadPtr < headPtr) ^ (nextTailPtr < tailPtr)

      for (i <- 0 until config.ifConfig.instFetchNum) {
        when (io.instsPack(i).valid) {
          fetchBufferReg(tailPtr + i) := io.instsPack(i)
        }
      }
    } else {
      // TODO: implement this
    }
    headPtr := nextHeadPtr
    tailPtr := nextTailPtr
  }

  for (i <- 0 until decodeWayNum) {
    io.insts2Decode(i).payload := fetchBufferReg(headPtr + i).payload
    io.insts2Decode(i).valid := Mux(U(i) >= occupiedNum, False, True)
    io.insts2Decode(i).pc := fetchBufferReg(headPtr + i).pc
  }

}

object genFetchBufferVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(FetchBuffer())
  }
}