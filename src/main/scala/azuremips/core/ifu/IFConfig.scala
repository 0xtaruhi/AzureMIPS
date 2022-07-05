package azuremips.core.ifu

import spinal.core._

import azuremips.core._

case class IFConfig(
  instFetchWidth: Int = 128,
  rasDepth: Int = 16,
  rasCounterWidth: Int = 4
) {
  val instFetchNum = instFetchWidth / 32
}