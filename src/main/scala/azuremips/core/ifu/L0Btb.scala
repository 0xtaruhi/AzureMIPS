package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

class L0BtbUpdateInfo extends Bundle {
  val valid  = Bool()
  val pc     = UInt(32 bits)
  val destPc = UInt(32 bits)
}

case class L0Btb(
  config: CoreConfig = CoreConfig()
  ) extends Component {
  val io = new Bundle {
    val pc         = in UInt(32 bits)
    val hit        = out Bool()
    val destPc     = out UInt(32 bits)
    val updateInfo = in(new L0BtbUpdateInfo)
  }

  class L0BtbEntry extends Bundle {
    // val valid  = Bool()
    val pc     = UInt(32 bits)
    val destPc = UInt(32 bits)
  }
  val l0btbDepth = config.ifConfig.l0btbDepth
  val l0btbMap = Vec(Reg(new L0BtbEntry), l0btbDepth)

  io.hit := l0btbMap.map(_.pc === io.pc).reduce(_ || _)
  io.destPc := l0btbMap.map(
    entry => Mux(entry.pc === io.pc, entry.destPc, U(0))
  ).reduce(_ | _)

  val tailPtr = RegInit(U(0, log2Up(l0btbDepth) bits))
  val existEntry = l0btbMap.map(_.pc === io.updateInfo.pc).reduce(_ || _)
  
  when (!existEntry) {
    l0btbMap(tailPtr).pc := io.updateInfo.pc
    l0btbMap(tailPtr).destPc := io.updateInfo.destPc
    if (isPow2(l0btbDepth)) {
      tailPtr := tailPtr + 1
    } else {
      when (tailPtr === l0btbDepth - 1) {
        tailPtr := 0
      } otherwise {
        tailPtr := tailPtr + 1
      }
    }
  } otherwise {
    for (entry <- l0btbMap) {
      when (entry.pc === io.updateInfo.pc) {
        entry.destPc := io.updateInfo.destPc
      }
    }
  }
}
