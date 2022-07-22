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

  def RS_BLTZ      = 0x00
  def RS_BGEZ      = 0x01
  def RS_BLTZAL    = 0x10
  def RS_BGEZAL    = 0x11
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
      uOpEret,uOpMfc0, uOpMtc0 = newElement()
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
  val exptCodeWidth    = 4
  val EXC_INTERCEPT    = 0
  val EXC_ADEL         = 4
  val EXC_ADES         = 5
  val EXC_SYSCALL      = 8
  val EXC_BREAK        = 9
  val EXC_RESERVED     = 10
  val EXC_OVF          = 12
}