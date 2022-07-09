package azuremips.core

import spinal.core._

object Mips {
  // I-Type Structure
  def opcodeRange      = 31 downto 26
  def rsRange          = 25 downto 21
  def rtRange          = 20 downto 16
  def immediateRange   = 15 downto 0
  def selRange         = 2  downto 0  // MFC0 and MTC0

  // J-Type Structuree
  def instr_indexRange = 25 downto 0

  // R-Type Structure
  def rdRange          = 15 downto 11
  def saRange          = 10 downto 6
  def functRange       = 5 downto 0

  // Instructions
  def INST_ADD      = M"000000---------------00000100000"
  def INST_ADDI     = M"001000--------------------------"
  def INST_ADDU     = M"000000---------------00000100001"
  def INST_ADDIU    = M"001001--------------------------"
  def INST_SUB      = M"000000---------------00000100010"
  def INST_SUBU     = M"000000---------------00000100011"
  def INST_SLT      = M"000000---------------00000101010"
  def INST_SLTI     = M"001010--------------------------"
  def INST_SLTU     = M"000000---------------00000101011"
  def INST_SLTIU    = M"001011--------------------------"
  def INST_DIV      = M"000000----------0000000000011010"
  def INST_DIVU     = M"000000----------0000000000011011"
  def INST_MULT     = M"000000----------0000000000011000"
  def INST_MULTU    = M"000000----------0000000000011001"

  def INST_AND      = M"000000---------------00000100100"
  def INST_ANDI     = M"001100--------------------------"
  def INST_LUI      = M"00111100000---------------------"
  def INST_NOR      = M"000000---------------00000100111"
  def INST_OR       = M"000000---------------00000100101"
  def INST_ORI      = M"001101--------------------------"
  def INST_XOR      = M"000000---------------00000100110"
  def INST_XORI     = M"001110--------------------------"

  def INST_SLLV     = M"000000---------------00000000100"
  def INST_SLL      = M"000000---------------00000000000"
  def INST_SRAV     = M"000000---------------00000000111"
  def INST_SRA      = M"000000---------------00000000011"
  def INST_SRLV     = M"000000---------------00000000110"
  def INST_SRL      = M"000000---------------00000000010"

  def INST_BEQ      = M"000100--------------------------"
  def INST_BNE      = M"000101--------------------------"
  def INST_BGEZ     = M"000001-----00001----------------"
  def INST_BGTZ     = M"000111-----00000----------------"
  def INST_BLEZ     = M"000110-----00000----------------"
  def INST_BLTZ     = M"000001-----00000----------------"
  def INST_BGEZAL   = M"000001-----10001----------------"
  def INST_BLTZAL   = M"000001-----10000----------------"
  def INST_J        = M"000011--------------------------"
  def INST_JR       = M"000000-----000000000000000001000"
  def INST_JALR     = M"000000-----00000-----00000001001"
  def INST_MFHI     = M"0000000000000000-----00000010000"
  def INST_MFLO     = M"0000000000000000-----00000010010"
  def INST_MTHI     = M"000000-----000000000000000010001"
  def INST_MTLO     = M"000000-----000000000000000010011"

  def INST_BREAK    = M"000000--------------------001101"
  def INST_SYSCALL  = M"000000--------------------001100"

  def INST_LB       = M"100000--------------------------"
  def INST_LBU      = M"100100--------------------------"
  def INST_LH       = M"100001--------------------------"
  def INST_LHU      = M"100101--------------------------"
  def INST_LW       = M"100011--------------------------"
  def INST_SB       = M"101000--------------------------"
  def INST_SH       = M"101001--------------------------"
  def INST_SW       = M"101011--------------------------"

  def INST_ERET     = M"01000010000000000000000000011000"
  def INST_MFC0     = M"01000000000----------00000000---"
  def INST_MTC0     = M"01000000100----------00000000---"

  // opcode
  def isBrInst(inst: UInt) = {
    inst(31 downto 29) === M"000" &&
      !(inst(28 to 26) === M"000" || inst(28 to 26) === M"001")
  }

  def isImmArithInst(inst: UInt) = {
    inst(31 downto 29) === M"001"
  }
  
  def isLoadInst(inst: UInt) = {
    inst(31 downto 29) === M"100"
  }

  def isStoreInst(inst: UInt) = {
    inst(31 downto 29) === M"101"
  }

  def isSpecialInst(inst: UInt) = {
    inst(opcodeRange) === M"000000"
  }

  def isSpecial2Inst(inst: UInt) = {
    inst(opcodeRange) === M"011100"
  }

  def OP_SPEC    = 0x00
  def OP_ADDI    = 0x08
  def OP_ADDIU   = 0x09 
  def OP_SLTI    = 0x0A
  def OP_SLTIU   = 0x0B
  def OP_ANDI    = 0x0C
  def OP_ORI     = 0x0D
  def OP_XORI    = 0x0E
  def OP_LUI     = 0x0F
  def OP_BEQ     = 0x04
  def OP_BNE     = 0x05
  def OP_REGIMM  = 0x01 //including BLTZ, BGEZ, BLTZAL, BGEZAL
  def OP_BGTZ    = 0x07
  def OP_BLEZ    = 0x06
  def OP_J       = 0x02
  def OP_JAL     = 0x03
  def OP_LB      = 0x20
  def OP_LBU     = 0x24
  def OP_LH      = 0x21
  def OP_LHU     = 0x25
  def OP_LW      = 0x23
  def OP_SB      = 0x28
  def OP_SH      = 0x29
  def OP_SW      = 0x2B
  def OP_PRIV    = 0x10 // privileged inst (ERET, MFC0, MTC0)

  def brInstOpList   = List(OP_BEQ, OP_BNE, OP_BLEZ, OP_BGTZ)
  def immInstOpList  = List(OP_ADDI, OP_ADDIU, OP_SLTI, OP_SLTIU, OP_ANDI, OP_LUI, OP_ORI, OP_XORI)
  def loadInstOpList = List(OP_LB, OP_LH, OP_LW, OP_LBU, OP_LHU)
  def stoInstOpList  = List(OP_SB, OP_SH, OP_SW)
  // funct
  def FUN_ADD      = 0x20
  def FUN_ADDU     = 0x21
  def FUN_SUB      = 0x22
  def FUN_SUBU     = 0x23
  def FUN_SLT      = 0x2A
  def FUN_SLTU     = 0x2B
  def FUN_DIV      = 0x1A
  def FUN_DIVU     = 0x1B
  def FUN_MULT     = 0x18
  def FUN_MULTU    = 0x19
  def FUN_AND      = 0x24
  def FUN_NOR      = 0x27
  def FUN_OR       = 0x25
  def FUN_XOR      = 0x26
  def FUN_SLLV     = 0x04
  def FUN_SLL      = 0x00
  def FUN_SRAV     = 0x07
  def FUN_SRA      = 0x03
  def FUN_SRLV     = 0x06
  def FUN_SRL      = 0x02
  def FUN_JR       = 0x08
  def FUN_JALR     = 0x09
  def FUN_MFHI     = 0x10
  def FUN_MFLO     = 0x12
  def FUN_MTHI     = 0x11
  def FUN_MTLO     = 0x13
  def FUN_BREAK    = 0x0D
  def FUN_SYSCALL  = 0x0C
  def specArLoInstFunctList = List(
    FUN_ADD,
    FUN_ADDU,
    FUN_SUB,
    FUN_SUBU,
    FUN_AND,
    FUN_OR,
    FUN_XOR,
    FUN_NOR,
    FUN_SLT,
    FUN_SLTU
  )

  def specShSaInstFunctList = List(
    FUN_SLL,  // sa
    FUN_SRL,  // sa
    FUN_SRA   // sa
  )

  def specShVInstFunctList = List(
    FUN_SLLV,
    FUN_SRLV,
    FUN_SRAV
    // FUN_JR,
    // FUN_JALR,
    // FUN_MFHI,
    // FUN_MFLO,
    // FUN_MTHI,
    // FUN_MTLO,
    // FUN_BREAK,
    // FUN_SYSCALL
  )

  def specMDInstFunctList = List(
    FUN_MULT,
    FUN_MULTU,
    FUN_DIV,
    FUN_DIVU
  )

  def RT_BLTZ   = 0x00
  def RT_BGEZ   = 0x01
  def RT_BLTZAL = 0x10
  def RT_BGEZAL = 0x11
  def regImmInstRtList = List(
    RT_BLTZ,
    RT_BGEZ,
    RT_BLTZAL,
    RT_BGEZAL
  )

  def RS_ERET = 0x10
  def RS_MFC0 = 0x00
  def RS_MTC0 = 0x04
  def privInstRsList = List(
    RS_ERET,
    RS_MFC0,
    RS_MTC0
  )
}

object Cp0 {
  def BadVAddrIdx = 8
  def CountIdx    = 9
  def StatusIdx   = 12
  def CauseIdx    = 13
}