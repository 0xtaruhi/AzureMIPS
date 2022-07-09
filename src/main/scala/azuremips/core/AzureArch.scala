package azuremips.core

import spinal.core._

object AzureConsts {
  val uopWidth: Int = 6
  val fuWidth: Int  = 4
}

object Uops {
  import Mips._
  import AzureConsts._
  def UOP_NOP   = 0 
  def ALU_ADD   = 1
  def ALU_SUB   = 2
  def ALU_SLT   = 3
  def ALU_SLTU  = 4
  def ALU_AND   = 5
  def ALU_NOR   = 6
  def ALU_LUI   = 7
  def ALU_OR    = 8
  def ALU_XOR   = 9
  def ALU_SLL   = 10
  def ALU_SRA   = 11
  def ALU_SRL   = 12
  def ALU_JAL   = 13
  def MD_MULT   = 14
  def MD_MULTU  = 15
  def MD_DIV    = 16
  def MD_DIVU   = 17
  def ALU_BLTZ  = 18
  def ALU_BGEZ  = 19
  def ALU_BEQ   = 20
  def ALU_BNE   = 21
  def ALU_BLEZ  = 22
  def ALU_BGTZ  = 23
  def LSU_LB    = 24
  def LSU_LH    = 25
  def LSU_LW    = 26
  def LSU_LBU   = 27
  def LSU_LHU   = 28
  def LSU_SB    = 29
  def LSU_SH    = 30
  def LSU_SW    = 31
  def ALU_CP0   = 32

  val immOpUopMap = Map[Int, Int](
    OP_ADDI  -> ALU_ADD,
    OP_ADDIU -> ALU_ADD,
    OP_SLTI  -> ALU_SLT,
    OP_SLTIU -> ALU_SLTU,
    OP_ANDI  -> ALU_AND,
    OP_ORI   -> ALU_OR,
    OP_XORI  -> ALU_XOR,
    OP_LUI   -> ALU_LUI
  )

  val brOpUopMap = Map[Int, Int](
    OP_BEQ  -> ALU_BEQ,
    OP_BNE  -> ALU_BNE,
    OP_BLEZ -> ALU_BLEZ,
    OP_BGTZ -> ALU_BGTZ
  )

  val loadOpUopMap = Map[Int, Int](
    OP_LB  -> LSU_LB,
    OP_LBU -> LSU_LBU,
    OP_LH  -> LSU_LH,
    OP_LHU -> LSU_LHU,
    OP_LW  -> LSU_LW
  )

  val stoOpUopMap = Map[Int, Int](
    OP_SB -> LSU_SB,
    OP_SH -> LSU_SH,
    OP_SW -> LSU_SW
  )

  val specArLoFunctUopMap = Map[Int, Int](
    FUN_ADD  -> ALU_ADD,
    FUN_ADDU -> ALU_ADD,
    FUN_SUB  -> ALU_SUB,
    FUN_SUBU -> ALU_SUB,
    FUN_AND  -> ALU_AND,
    FUN_OR   -> ALU_OR,
    FUN_XOR  -> ALU_XOR,
    FUN_NOR  -> ALU_NOR,
    FUN_SLT  -> ALU_SLT,
    FUN_SLTU -> ALU_SLTU
  )

  val specShSaFunctUopMap = Map[Int, Int](
    FUN_SLL  -> ALU_SLL,
    FUN_SRA  -> ALU_SRA,
    FUN_SRL  -> ALU_SRL
  )

  val specShVFunctUopMap = Map[Int, Int](
    FUN_SLLV -> ALU_SLL,
    FUN_SRLV -> ALU_SRL,
    FUN_SRAV -> ALU_SRA
    // FUN_JR   -> ALU_ADD,
    // FUN_JALR -> ALU_ADD,
    // FUN_MFHI -> 
  )

  val specMDFunctUopMap = Map[Int, Int](
    FUN_MULT  -> MD_MULT,
    FUN_MULTU -> MD_MULTU,
    FUN_DIV   -> MD_DIV,
    FUN_DIVU  -> MD_DIVU
  )

  val regImmRtUopMap = Map[Int, Int](
    RT_BLTZ   -> ALU_BLTZ,
    RT_BGEZ   -> ALU_BGEZ,
    RT_BLTZAL -> ALU_BLTZ,
    RT_BGEZAL -> ALU_BGEZ
  )
}

object FU {
  def FU_LSU = 0
  def FU_ALU = 1
  def FU_MD  = 2
}