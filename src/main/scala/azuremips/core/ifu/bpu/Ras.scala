package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._

class RasEntry extends Bundle {
  val valid = Bool()
  val data  = UInt(32 bits)
}

case class Ras(depth: Int = 8) extends Component {
  val io = new Bundle {
    val flush    = in Bool()
    val pushEn   = in Bool()
    val pushData = in UInt(32 bits)
    val popEn    = in Bool()
    val topData  = out UInt(32 bits)
    val topValid = out Bool()
  }

  val ras = Vec(Reg(new RasEntry), depth)
  for (rasEntry <- ras) {
    rasEntry.valid init (False)
    when (io.flush) {
      rasEntry.valid := False
    }
  }
  val topPtr = Reg(UInt(log2Up(depth) bits)) init (0)
  io.topValid := ras(topPtr).valid
  io.topData := ras(topPtr).data

  when (io.pushEn) {
    ras(topPtr + 1).valid := True
    ras(topPtr + 1).data := io.pushData
    topPtr := topPtr + 1
  }

  when (io.popEn) {
    ras(topPtr).valid := False
    topPtr := topPtr - 1
  }

  when (io.pushEn && io.popEn) {
    // topPtr := topPtr
    ras(topPtr).valid := True
    ras(topPtr).data := io.pushData
  }
}

object GenRasVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Ras(8))
  }
}