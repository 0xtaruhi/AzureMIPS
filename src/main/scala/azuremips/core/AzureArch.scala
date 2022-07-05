package azuremips.core

import spinal.core._

object AzureConsts {
  val uopWidth: Int = 4
  val fuWidth: Int  = 4
}

object Uops {
  import Mips._
  import AzureConsts._
  def UOP_NOP  = 0 
  def ALU_ADD  = 1
  def ALU_SUB  = 2
  def ALU_SLT  = 3
  def ALU_SLTU = 4
  def ALU_AND  = 5
  def ALU_NOR  = 6
  def ALU_LUI  = 7
  def ALU_OR   = 8
  def ALU_XOR  = 9
  def ALU_SLL  = 10
  def ALU_SRA  = 11
  def ALU_SRL  = 12
  

  val opcodeUopMap = Map[Int, Int](
    OP_ADDI  -> ALU_ADD,
    OP_ADDIU -> ALU_ADD,
    OP_SLTI  -> ALU_SLT,
    OP_SLTIU -> ALU_SLTU,
    OP_ANDI  -> ALU_AND,
    OP_ORI   -> ALU_OR,
    OP_XORI  -> ALU_XOR,
    OP_LUI   -> ALU_LUI
  )

  val specFunctUopMap = Map[Int, Int](
    FUN_ADD  -> ALU_ADD,
    FUN_ADDU -> ALU_ADD,
    FUN_SUB  -> ALU_SUB,
    FUN_SUBU -> ALU_SUB,
    FUN_AND  -> ALU_AND,
    FUN_OR   -> ALU_OR,
    FUN_XOR  -> ALU_XOR,
    FUN_NOR  -> ALU_NOR,
    FUN_SLT  -> ALU_SLT,
    FUN_SLTU -> ALU_SLTU,
    FUN_SLL  -> ALU_SLL,
    FUN_SRA  -> ALU_SRA,
    FUN_SRL  -> ALU_SRL,
    FUN_SRLV -> ALU_SRL,
    FUN_SRAV -> ALU_SRA
    // FUN_JR   -> ALU_ADD,
    // FUN_JALR -> ALU_ADD,
    // FUN_MFHI -> 
  )
}

object FU {
  def FU_LSU = 0
  def FU_ALU = 1
  def FU_MD  = 2
}