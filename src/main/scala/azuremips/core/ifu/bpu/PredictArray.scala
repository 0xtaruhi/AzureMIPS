package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ifu.bpu.TwoBitCounterStatus._

case class PredictArray() extends Component with BhtConfig with GhrConfig {
  val io = new Bundle {
    val pc            = in UInt(32 bits)
    val ghr           = in UInt(ghrSize bits)
    val inTakenArray  = in Bool()    
    val predictTaken  = out Bool() setAsReg

    val updateEn      = in Bool()
    val updateInTaken = in Bool()
    val updatePc      = in UInt(32 bits)
    val updateTaken   = in Bool()
  }

  def getOffset(pc : UInt, ghr : UInt) = {
    val offset = pc((2 + offsetWidth) downto 3) ^ ghr((offsetWidth - 1) downto 0)
    offset
  }

  def getIndex(pc : UInt, ghr : UInt) = {
    val index = ghr(9 downto 8) @@ (ghr(7 downto 4) ^ ghr(3 downto 0))
    // val index = (ghr(7 downto 6) ^ ghr(5 downto 4)) @@ ghr(3 downto 0) // 8 + hi
    // val index = ghr(5 downto 4) @@ (ghr(3 downto 2) ^ ghr(1 downto 0))
    // val index = U(ghr(5) ^ ghr(4)) @@ ghr(3 downto 0)
    // val index = ghr
    index
  }

  val takenArray = Vec(
    Vec(TwoBitCounter(WeaklyTaken), 1 << offsetWidth), 1 << indexWidth
  )
  val notTakenArray = Vec(
    Vec(TwoBitCounter(WeaklyNotTaken), 1 << offsetWidth), 1 << indexWidth
  )

  val selectArea = new Area {
    val index  = getIndex(io.pc, io.ghr)
    val offset = getOffset(io.pc, io.ghr)
    
    val takenArrayResult = takenArray(index)(offset).predictTaken
    val notTakenArrayResult = notTakenArray(index)(offset).predictTaken

    io.predictTaken := Mux(io.inTakenArray, takenArrayResult, notTakenArrayResult)
  }

  val updateArea = new Area {
    val index  = getIndex(io.updatePc, io.ghr)
    val offset = getOffset(io.updatePc, io.ghr)
    
    when (io.updateEn) {
      takenArray(index)(offset).updateWhen(io.updateTaken, io.updateInTaken)
      notTakenArray(index)(offset).updateWhen(io.updateTaken, !io.updateInTaken)
    }
  }
}

object GenPredictArrayVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(PredictArray())
  }
}