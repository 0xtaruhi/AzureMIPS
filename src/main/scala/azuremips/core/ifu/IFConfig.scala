package azuremips.core.ifu

import spinal.core._

import azuremips.core._

case class IFConfig(
  instFetchWidth: Int = 128,
  rasDepth: Int = 16,
  rasCounterWidth: Int = 4,
  fetchBufferDepth: Int = 8,
  l0btbDepth: Int = 16
) {
  val instFetchNum = instFetchWidth / 32
}