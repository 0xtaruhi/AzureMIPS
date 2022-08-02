package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._

case class PredictArray() extends Component with BhtConfig with GhrConfig {
  val io = new Bundle {
    val pc = in UInt(32 bits)
    val ghr = in UInt(ghrSize bits)
    
    val predictTaken = out Bool()
  }

  
}