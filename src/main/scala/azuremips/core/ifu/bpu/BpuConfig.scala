package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._

trait GhrConfig {
  val ghrSize = 10
}

trait SelectArrayConfig {
  val offsetWidth = 3
  val indexWidth  = 5

  val pcOffsetRange = ((offsetWidth + 1) downto 2)
  val pcIndexRange  = ((offsetWidth + indexWidth + 1) downto (offsetWidth + 2))
}

trait BhtConfig {
  val offsetWidth = 4
  val indexWidth  = 6
}