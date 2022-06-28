package azuremips

import spinal.core._
import spinal.lib._

import azuremips.core._

case class SocConfig(
  coreConfig: CoreConfig = CoreConfig()
)

case class Soc(config: SocConfig) extends Component {
  val io = new Bundle {

  }
}