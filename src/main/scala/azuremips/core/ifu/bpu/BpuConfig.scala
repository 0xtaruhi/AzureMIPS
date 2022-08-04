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

trait BtbConfig {
  val indexUpperBound = 13
  val indexLowerBound = 4
  val tagUpperBound = 19

  // params caculation
  val validWidth = 1
  val tagLowerBound = indexUpperBound + 1
  val tagWidth = tagUpperBound + 1 - tagLowerBound
  val indexWidth = indexUpperBound + 1 - indexLowerBound
  val btbSize = scala.math.pow(2, indexWidth).toInt
  assert(tagWidth > 0)
  assert(tagUpperBound <= 30)
}