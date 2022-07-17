package azuremips.core.ifu

import spinal.core._
import spinal.lib._
import azuremips.core._

class RasEntry extends Bundle {
  val valid = Bool()
  val data  = UInt(32 bits)
}

class Ras(depth: Int = 8) extends Component {
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
  io.topValid := ras(topPtr - 1).valid
  io.topData := ras(topPtr - 1).data

  when (io.pushEn) {
    topPtr := topPtr + 1
    ras(topPtr).valid := True
    ras(topPtr).data := io.pushData
  }

  when (io.popEn) {
    topPtr := topPtr - 1
    ras(topPtr - 1).valid := False
  }

  when (io.pushEn && io.popEn) {
    topPtr := topPtr
    ras(topPtr - 1).valid := True
    ras(topPtr - 1).data := io.pushData
  }
}

object GenRasVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Ras(8))
  }
}