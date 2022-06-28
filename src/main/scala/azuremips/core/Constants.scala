package azuremips.core

import spinal.core._

object Uops extends SpinalEnum {
  def uopWidth = 4
  val ALU_ADD = 0x0
  val ALU_SUB = 0x1
  val ALU_SLT = 0x2
  val ALU_NOR = 0x3
  val ALU_AND = 0x4
  val ALU_OR  = 0x5
  val ALU_SLL = 0x6
  val ALU_SRL = 0x7
}