package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ifu.bpu.TwoBitCounterStatus._

case class SelectArray() extends Component with SelectArrayConfig {
  val io = new Bundle {
    val pc           = in UInt(32 bits)
    val inTakenArray = out Bool() setAsReg
    val updateEn     = in Bool()
    val updatePc     = in UInt(32 bits)
    val updateTaken  = in Bool()
  }

  val selectArray = Vec(
    Vec(TwoBitCounter(WeaklyNotTaken), 1 << offsetWidth), 1 << indexWidth)

  val selectArea = new Area {
    val index  = io.pc(pcIndexRange)
    val offset = io.pc(pcOffsetRange)
    io.inTakenArray := selectArray(index)(offset).predictTaken
  }

  val updateArea = new Area {
    val index  = io.updatePc(pcIndexRange)
    val offset = io.updatePc(pcOffsetRange)
    selectArray(index)(offset).updateWhen(io.updateTaken, io.updateEn)
  }

}

object SelectArray {
  def main(args: Array[String]) {
    SpinalVerilog(new SelectArray)
  }
}