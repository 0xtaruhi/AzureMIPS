package azuremips.core.idu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.Instructions._

class DecodedSignals extends Bundle {
  val validInst  = Bool()
  val pc         = UInt(32 bits)
  val op1Addr    = UInt(5 bits)
  val op1RdGeRf  = Bool()
  val op2Addr    = UInt(5 bits)
  val op2RdGeRf  = Bool()
  val wrRegAddr  = UInt(5 bits)
  val wrRegEn    = Bool()
  val uop        = Uops()
  val useImm     = Bool()
  val imm        = UInt(32 bits)
  val isPriv     = Bool()
  val multiCycle = Bool()

  // val readHilo   = Bool()
  // val isHi       = Bool()

  def rawConflict(addr: UInt): Bool = {
    (addr === op1Addr) && op1RdGeRf || (addr === op2Addr) && op2RdGeRf
  }
}

class Decoder extends Component {
  val io = new Bundle {
    val flush   = in Bool()
    val pc      = in UInt(32 bits)
    val inst    = in UInt(32 bits)
    val signals = out(new DecodedSignals)
  }

  val inst = Mux(io.flush, U(0, 32 bits), io.inst)
  val opcode = inst(31 downto 26)
  val funct  = inst(5  downto 0 )
  val rs     = inst(25 downto 21)
  val rt     = inst(20 downto 16)
  val rd     = inst(15 downto 11)
  val imm    = inst(15 downto 0 )
  val sa     = inst(10 downto 6 )

  val uop = Uops()
  uop := uOpSll
  io.signals.validInst := True
  io.signals.pc      := io.pc
  io.signals.useImm  := False
  io.signals.uop     := uop
  io.signals.op1RdGeRf := True
  io.signals.op2RdGeRf := True
  io.signals.wrRegEn := True
  io.signals.isPriv  := False
  io.signals.multiCycle := False
  io.signals.op1Addr  := rs
  io.signals.op2Addr  := rt
  io.signals.wrRegAddr := rd
  // io.signals.readHilo := False
  // io.signals.isHi     := False
  
  val sextImm = U((15 downto 0) -> imm.msb) @@ imm
  val uextImm = U(0, 16 bits) @@ imm
  val brOffset = U((13 downto 0) -> imm.msb) @@ imm @@ U"00"
  val jTarget = (io.pc + 4)(31 downto 28) @@ inst(25 downto 0) @@ U"00"
  val saImm   = U(0, 27 bits) @@ sa
  val signExt = True
  val extImm  = UInt(32 bits)
  extImm := sextImm
  io.signals.imm := extImm

  switch (opcode) {
    is (OP_SPEC) {
      switch (funct) {
        is (FUN_ADD)  { uop := uOpAdd  }
        is (FUN_ADDU) { uop := uOpAddu }
        is (FUN_SUB)  { uop := uOpSub  }
        is (FUN_SUBU) { uop := uOpSubu }
        is (FUN_AND)  { uop := uOpAnd  }
        is (FUN_OR)   { uop := uOpOr   }
        is (FUN_XOR)  { uop := uOpXor  }
        is (FUN_NOR)  { uop := uOpNor  }
        is (FUN_SLL)  { uop := uOpSll  }
        is (FUN_SRL)  { uop := uOpSrl  }
        is (FUN_SRA)  { uop := uOpSra  }
        is (FUN_SLLV) { uop := uOpSllv }
        is (FUN_SRLV) { uop := uOpSrlv }
        is (FUN_SRAV) { uop := uOpSrav }
        is (FUN_JR)   { uop := uOpJr   }
        is (FUN_JALR) { uop := uOpJalr ; io.signals.wrRegAddr := 31 }
        is (FUN_SYSCALL) { uop := uOpSyscall }
        is (FUN_BREAK){ uop := uOpBreak }
        is (FUN_MFHI) { uop := uOpMfhi }
        is (FUN_MTHI) { uop := uOpMthi }
        is (FUN_MFLO) { uop := uOpMflo }
        is (FUN_MTLO) { uop := uOpMtlo }
        is (FUN_MULT) { uop := uOpMult  ; io.signals.multiCycle := True }
        is (FUN_MULTU){ uop := uOpMultu ; io.signals.multiCycle := True }
        is (FUN_DIV)  { uop := uOpDiv   ; io.signals.multiCycle := True }
        is (FUN_DIVU) { uop := uOpDivu  ; io.signals.multiCycle := True }
        is (FUN_SLT)  { uop := uOpSlt  }
        is (FUN_SLTU) { uop := uOpSltu }
        default {
          io.signals.validInst := False
        }
      }
    }
    is (OP_PRIV) {
      io.signals.isPriv := True
      io.signals.op1RdGeRf := False
      switch (rs) {
        is (U"00000") { uop := uOpMfc0 }
        is (U"00100") { uop := uOpMtc0 ; io.signals.wrRegEn := False}
        default { io.signals.validInst := False }
      }
      when (inst(25 downto 24) === U"00" && funct === U"011000") {
        uop := uOpEret
      } otherwise {
        io.signals.validInst := False
      }
    }
    is (OP_REGIMM) {
      switch (rt) { // todo
        is (RS_BGEZ)   { uop := uOpBgez   }
        is (RS_BGEZAL) { uop := uOpBgezal }
        is (RS_BLTZ)   { uop := uOpBltz   } 
        is (RS_BLTZAL) { uop := uOpBltzal }
      }
    }
    is (OP_ADDI  ) { uop := uOpAdd  } 
    is (OP_ADDIU ) { uop := uOpAddu }
    is (OP_SLTI  ) { uop := uOpSlt  }
    is (OP_SLTIU ) { uop := uOpSltu }
    is (OP_ANDI  ) { uop := uOpAnd  }
    is (OP_ORI   ) { uop := uOpOr   }
    is (OP_XORI  ) { uop := uOpXor  }
    is (OP_LUI   ) { uop := uOpLui  }
    is (OP_BEQ   ) { uop := uOpBeq  }
    is (OP_BNE   ) { uop := uOpBne  }
    is (OP_BGTZ  ) { uop := uOpBgtz }
    is (OP_BLEZ  ) { uop := uOpBlez }
    is (OP_J     ) { uop := uOpJ    }
    is (OP_JAL   ) { uop := uOpJal  ; io.signals.wrRegAddr := 31 }
    is (OP_LB    ) { uop := uOpLb   }
    is (OP_LBU   ) { uop := uOpLbu  }
    is (OP_LH    ) { uop := uOpLh   }
    is (OP_LHU   ) { uop := uOpLhu  }
    is (OP_LW    ) { uop := uOpLw   }
    is (OP_SB    ) { uop := uOpSb   }
    is (OP_SH    ) { uop := uOpSh   }
    is (OP_SW    ) { uop := uOpSw   }
    default {
      io.signals.validInst := False
    }
  }

  switch (opcode) {
    is (OP_ADDI, OP_ADDIU, OP_SLTI, OP_SLTIU) {
      io.signals.useImm    := True
      extImm    := sextImm
      io.signals.wrRegAddr  := rt
      io.signals.op2RdGeRf := False
    }
    is (OP_ANDI, OP_ORI, OP_XORI, OP_LUI) {
      io.signals.useImm    := True
      extImm    := uextImm
      io.signals.wrRegAddr  := rt
      io.signals.op2RdGeRf := False
    }
    is (OP_BEQ, OP_BNE, OP_BGTZ, OP_BLEZ) {
      // io.signals.useImm    := True
      extImm    := brOffset
      io.signals.wrRegEn   := False
    }
    is (OP_J) {
      io.signals.useImm    := True
      extImm    := jTarget
      io.signals.op1RdGeRf := False
      io.signals.op2RdGeRf := False
      io.signals.wrRegEn   := False
    }
    is (OP_JAL) {
      io.signals.useImm    := True
      extImm               := jTarget
      io.signals.op1RdGeRf := False
      io.signals.op2RdGeRf := False
      io.signals.wrRegEn   := True
    }
    is (OP_LB, OP_LBU, OP_LH, OP_LHU, OP_LW) {
      // io.signals.useImm    := True
      extImm    := sextImm
      io.signals.wrRegAddr  := rt
      io.signals.op2RdGeRf := False
    }
    is (OP_SB, OP_SH, OP_SW) {
      // io.signals.useImm    := True
      extImm    := sextImm
      io.signals.wrRegEn   := False
    }

    is (OP_REGIMM) {
      switch (rt) {
        is (RS_BGEZ, RS_BLTZ) {
          // io.signals.useImm    := True
          extImm               := brOffset
          io.signals.op2RdGeRf := False
          io.signals.wrRegEn   := False
        }
        is (RS_BGEZAL, RS_BLTZAL) {
          // io.signals.useImm    := True
          extImm               := brOffset
          io.signals.op2RdGeRf := False
          io.signals.wrRegAddr  := 31
        }
      }
    }

    is (OP_SPEC) {
      switch (funct) {
        is (FUN_SLL, FUN_SRL, FUN_SRA) {
          io.signals.useImm    := False
          extImm               := saImm
          io.signals.op1RdGeRf := False
        }
        is (FUN_MULT, FUN_MULTU, FUN_DIV, FUN_DIVU) {
          io.signals.multiCycle:= True
          io.signals.wrRegEn   := False
        }
        is (FUN_JR) {
          io.signals.op2RdGeRf := False
          io.signals.wrRegEn   := False
        }
        is (FUN_JALR) {
          io.signals.op2RdGeRf := False
          io.signals.wrRegAddr := 31
        }
      }
    }
  }

  when (io.flush) {
    io.signals.validInst := True
    io.signals.uop       := uOpSll
    io.signals.useImm    := False
    io.signals.pc        := 0
    io.signals.op1RdGeRf := False
    io.signals.op2RdGeRf := False
    io.signals.isPriv    := False
    io.signals.wrRegEn   := False
    io.signals.multiCycle:= False
    io.signals.op1Addr   := 0
    io.signals.op2Addr   := 0
    io.signals.wrRegAddr := 0
    io.signals.imm       := 0
  }
}

object GenDecoderVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Decoder)
  }
}