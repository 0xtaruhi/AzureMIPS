package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class FetchBuffer(
  config: CoreConfig = CoreConfig()
  ) extends Component {
  val io = new Bundle {
    val instPack     = in Vec(Flow(UInt(32 bits)), config.ifConfig.instFetchNum)
    val almostFull   = out Bool()
    val full         = out Bool()
    val insts2Decode = out Vec(UInt(32 bits), config.idConfig.decodeWayNum)
  }

  val fetchBufferReg = Vec(Reg(UInt(32 bits)) init(0), config.ifConfig.fetchBufferDepth)
  val fetchBufferAddrWidth = log2Up(config.ifConfig.fetchBufferDepth)

  val headPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val tailPtr = Reg(UInt(fetchBufferAddrWidth bits)) init(0)
  val availNum = Reg(UInt(log2Up(
    config.ifConfig.fetchBufferDepth + 1) bits)
    ) init(config.ifConfig.fetchBufferDepth)
  val occupiedNum = config.ifConfig.fetchBufferDepth - availNum

  io.almostFull := (availNum < config.ifConfig.instFetchNum)
  io.full       := (availNum === 0)

  val instPackValidInstNum = UInt(log2Up(config.ifConfig.instFetchNum) bits)
  instPackValidInstNum := io.instPack.map(_.valid.asUInt.resize(instPackValidInstNum.getWidth)).reduce(_ + _)

  def binaryListAddBit(list: List[List[Int]]): List[List[Int]] = {
    if (list.isEmpty) {
      List(List(0), List(1))
    } else {
      list.map(x => x :+ 0) ++ list.map(x => x :+ 1)
    }
  }
  val binaryList = {
    var list: List[List[Int]] = List(List())
    for (i <- 1 to config.ifConfig.instFetchNum) {
      list = binaryListAddBit(list)
    }
    list
  }

  val newInstsAllocatedAddr = Vec(UInt(fetchBufferAddrWidth bits), config.ifConfig.instFetchNum)

  for (i <- 0 until config.ifConfig.instFetchNum) {
    if (isPow2(config.ifConfig.fetchBufferDepth)) {
      newInstsAllocatedAddr(i) := headPtr - i
    } else {
      when (headPtr < U(i)) {
        newInstsAllocatedAddr(i) := config.ifConfig.fetchBufferDepth + headPtr - i
      } otherwise {
        newInstsAllocatedAddr(i) := headPtr - i
      }
    }
  }

  switch (Cat(io.instPack.map(_.valid.asUInt))) {
    for (binaryCode <- binaryList) yield {
      is (Cat(binaryCode.map(x => U(x, 1 bits)))) {
        val validInstNum = binaryCode.reduce(_ + _)
        val validInstPack = Vec(UInt(32 bits), validInstNum)
        var j = 0
        for (i <- 0 until binaryCode.length) {
          if (binaryCode(i) == 1) {
            validInstPack(j) := io.instPack(i).payload
            j = j + 1
          }
        }
        for (i <- 0 until validInstNum) {
          fetchBufferReg(newInstsAllocatedAddr(i)) := validInstPack(i)
        }
        headPtr := headPtr + validInstNum
      }
    }
  }

  // to Decode
  for (i <- 0 until config.idConfig.decodeWayNum) {
    if (isPow2(config.ifConfig.fetchBufferDepth)) {
      io.insts2Decode(i) := fetchBufferReg(tailPtr - i)
    } else {
      when (tailPtr < U(i)) {
        io.insts2Decode(i) := fetchBufferReg(config.ifConfig.fetchBufferDepth + tailPtr - i)
      } otherwise {
        io.insts2Decode(i) := fetchBufferReg(tailPtr - i)
      }
    }
  }
  tailPtr := tailPtr - config.idConfig.decodeWayNum

}
