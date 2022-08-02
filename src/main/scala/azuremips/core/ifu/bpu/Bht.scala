package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class Bht() extends Component with GhrConfig {
  val io = new Bundle {
    val pc           = in UInt(32 bits)
    val updateEn     = in Bool()
    val updatePc     = in UInt(32 bits)
    val updateTaken  = in Bool()
    val predictTaken = out Bool()
  }

  val ghr = RegInit(U(0, ghrSize bits))

  val selectArray  = SelectArray()
  val predictArray = PredictArray()

  when (io.updateEn) {
    ghr := ghr((ghrSize - 2) downto 0) @@ io.updateTaken
  }

  selectArray.io.pc := io.pc
  selectArray.io.updateEn := io.updateEn
  selectArray.io.updatePc := io.updatePc
  selectArray.io.updateTaken := io.updateTaken

  predictArray.io.pc  := io.pc
  predictArray.io.ghr := ghr
  predictArray.io.updateEn  := io.updateEn
  predictArray.io.updatePc  := io.updatePc
  predictArray.io.inTakenArray  := selectArray.io.inTakenArray
  predictArray.io.updateInTaken := selectArray.io.updateInTaken
  predictArray.io.updateTaken   := io.updateTaken

  io.predictTaken := predictArray.io.predictTaken
}

object GenBhtVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(Bht())
  }
}