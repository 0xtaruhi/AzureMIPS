package azuremips.core

import spinal.core._
import spinal.lib._

object AzureConsts {
  val uopWidth: Int = 6
  val fuWidth: Int  = 4
  val vaddrWidth: Int = 32
  val paddrWidth: Int = 32
}

object Instructions {
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
  def OP_BEQL    = 0x14
  def OP_BNEL    = 0x15
  def OP_BGTZL   = 0x17
  def OP_BLEZL   = 0x16
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
  def OP_COP0    = 0x10 // privileged inst (ERET, MFC0, MTC0)
  def OP_COP1    = 0x11
  def OP_SPEC2   = 0x1C
  def OP_LWL     = 0x22
  def OP_LWR     = 0x26
  def OP_SWL     = 0x2A
  def OP_SWR     = 0x2E
  def OP_LDC1    = 0x35
  def OP_SDC1    = 0x3D
  def OP_CACHE   = 0x2F
  def OP_LL      = 0x30
  def OP_PREF    = 0x33
  def OP_SC      = 0x38

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
  def FUN_MOVZ     = 0x0A
  def FUN_MOVN     = 0x0B
  def FUN_MFHI     = 0x10
  def FUN_MFLO     = 0x12
  def FUN_MTHI     = 0x11
  def FUN_MTLO     = 0x13
  def FUN_BREAK    = 0x0D
  def FUN_SYSCALL  = 0x0C
  def FUN_MADD     = 0x00
  def FUN_MADDU    = 0x01
  def FUN_MUL      = 0x02
  def FUN_MSUB     = 0x04
  def FUN_MSUBU    = 0x05
  def FUN_ERET     = 0x18
  def FUN_TLBP     = 0x08
  def FUN_TLBR     = 0x01
  def FUN_TLBWI    = 0x02
  def FUN_TLBWR    = 0x06
  def FUN_DERET    = 0x1f
  def FUN_WAIT     = 0x20
  def FUN_SYNC     = 0x0f

  def RT_BLTZ      = 0x00
  def RT_BGEZ      = 0x01
  def RT_BLTZAL    = 0x10
  def RT_BGEZAL    = 0x11
  def RT_BLTZL     = 0x02
  def RT_BGEZL     = 0x03
  def RT_BLTZALL   = 0x12
  def RT_BGEZALL   = 0x13
  def RS_MFC0      = 0x00
  def RS_MTC0      = 0x04
  def RS_MFC1      = 0x00
  def RS_CFC1      = 0x02
  def RS_MTC1      = 0x04
  def RS_CTC1      = 0x06
  def RT_ICACHEII  = 0x00
  def RT_ICACHEIST = 0x08
  def RT_ICACHEHI  = 0x10
  def RT_ICACHEFILL= 0x14
  def RT_DCACHEIWI = 0x01
  def RT_DCACHEIST = 0x09
  def RT_DCACHEHI  = 0x11
  def RT_DCACHEHWI = 0x15
}

object Uops extends SpinalEnum {
  val uOpAdd, uOpAddu, uOpSub,  uOpSubu,
      uOpSlt, uOpSltu,
      uOpDiv, uOpDivu, uOpMult, uOpMultu,
      uOpAnd, uOpLui,  uOpNor,  uOpOr,    uOpXor,  
      uOpSllv,uOpSll,  uOpSrav, uOpSra,   uOpSrlv, uOpSrl,
      uOpBeq, uOpBne,  uOpBgez, uOpBgtz,  uOpBlez, uOpBltz, uOpBgezal, uOpBltzal,
      uOpJ,   uOpJal,  uOpJr,   uOpJalr,
      uOpMfhi,uOpMflo, uOpMthi, uOpMtlo, 
      uOpBreak,uOpSyscall,
      uOpLb,  uOpLbu,  uOpLh,   uOpLhu,   uOpLw,   uOpSb,   uOpSh,     uOpSw,
      uOpLl,  uOpSc,   uOpSwl,  uOpSwr,  uOpLwl,   uOpLwr,
      uOpEret,uOpMfc0, uOpMtc0,
      uOpTlbp,uOpTlbr, uOpTlbwi,
      uOpMovz, uOpMovn,
      uOpDCacheIWI, uOpDCacheIST, uOpDCacheHI, uOpDCacheHWI,
      uOpICacheII,  uOpICacheIST, uOpICacheHI, uOpICacheFill,
      uOpMul = newElement()
}

class BypassPort extends Bundle {
  val wrRegEn   = Bool()
  val wrRegAddr = UInt(5 bits)
  val wrData    = UInt(32 bits)
  val isLoad    = Bool()

  def hit(regAddr: UInt): Bool = {
    wrRegAddr === regAddr && wrRegEn && regAddr =/= U(0)
  }
  def stall(regAddr: UInt): Bool = {
    isLoad && hit(regAddr)
  }
}

// class ExBypassPort extends BypassPort {
//   val isLoad    = Bool()
  

// }

object ExceptionCode {
  val exptCodeWidth    = 5
  val EXC_INTERRUPT    = 0x0
  val EXC_TLBMOD       = 0x1
  val EXC_TLBREFILL_L  = 0x2
  val EXC_TLBREFILL_S  = 0x3
  val EXC_TLBINVALID_L = 0x4
  val EXC_TLBINVALID_S = 0x5
  val EXC_ADEL         = 0x6
  val EXC_ADES         = 0x7
  val EXC_SYSCALL      = 0x8
  val EXC_BREAK        = 0x9
  val EXC_RESERVED     = 0xa
  val EXC_OVF          = 0xb
  val EXC_ERET         = 0xc
  val EXC_ADEL_FI      = 0xd
  val EXC_CP1_UNUSABLE = 0xe
}