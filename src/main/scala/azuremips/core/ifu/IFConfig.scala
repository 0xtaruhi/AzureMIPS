package azuremips.core.ifu

import spinal.core._

import azuremips.core._

case class IFConfig(
  instFetchWidth: Int = 128,
  rasDepth: Int = 16,
  rasCounterWidth: Int = 4,
  fetchBufferDepth: Int = 8,
  l0btbDepth: Int = 16

  bhtUpperBound: Int = 10,
  bhtLowerBound: Int = 2,
) {
  val instFetchNum = instFetchWidth / 32
  val bhtAddrWidth = bhtUpperBound + 1 - bhtLowerBound
  val bhtDepth = scala.math.pow(2, bhtAddrWidth).toInt
}