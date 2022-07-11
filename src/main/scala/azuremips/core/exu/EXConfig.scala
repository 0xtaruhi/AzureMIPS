package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class EXConfig (
  aluNum: Int = 4,
  lsuNum: Int = 2,
  mduNum: Int = 1,
  bruNum: Int = 1
) {
  val fuNum = aluNum + lsuNum + mduNum + bruNum
}