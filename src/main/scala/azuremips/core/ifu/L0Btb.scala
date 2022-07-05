package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class L0Btb(
  config: CoreConfig = CoreConfig())
  extends Component {

  val io = new Bundle {
    val pc     = in UInt(32 bits)
    val hit    = out Bool()
    val destPc = out UInt(32 bits)
  }
}