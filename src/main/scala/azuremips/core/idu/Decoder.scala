package azuremips.core.idu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.Instructions._

case class DecodedSignals() extends Bundle {
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
  val useHilo    = Bool()
  val isPriv     = Bool()
  val multiCycle = Bool()
  val isBr       = Bool()
  val cp0Addr    = UInt(5 bits)
  // val cp0Sel     = UInt(3 bits)

  def rawConflict(addr: UInt): Bool = {
    (addr === op1Addr) && op1RdGeRf || (addr === op2Addr) && op2RdGeRf
  }

  def nopDecodedSignals = {
    val s = DecodedSignals()
    s.validInst := True
    s.pc        := 0
    s.op1Addr   := 0
    s.op1RdGeRf := False
    s.op2Addr   := 0
    s.op2RdGeRf := False
    s.wrRegAddr := 0
    s.wrRegEn   := False
    s.uop       := uOpSll
    s.useImm    := False
    s.imm       := 0
    s.useHilo   := False
    s.isPriv    := False
    s.multiCycle := False
    s.isBr      := False
    s.cp0Addr   := 0
    s
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
  io.signals.validInst  := True
  io.signals.pc         := io.pc
  io.signals.useImm     := False
  io.signals.uop        := uop
  io.signals.op1RdGeRf  := True
  io.signals.op2RdGeRf  := True
  io.signals.wrRegEn    := True
  io.signals.isPriv     := False
  io.signals.useHilo    := False
  io.signals.multiCycle := False
  io.signals.op1Addr    := rs
  io.signals.op2Addr    := rt
  io.signals.wrRegAddr  := rd
  io.signals.isBr       := False
  io.signals.cp0Addr    := rd
  
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
        is (FUN_MFHI) { uop := uOpMfhi  ; io.signals.useHilo := True }
        is (FUN_MTHI) { uop := uOpMthi  ; io.signals.useHilo := True }
        is (FUN_MFLO) { uop := uOpMflo  ; io.signals.useHilo := True }
        is (FUN_MTLO) { uop := uOpMtlo  ; io.signals.useHilo := True }
        is (FUN_MULT) { uop := uOpMult  ; io.signals.useHilo := True ; io.signals.multiCycle := True }
        is (FUN_MULTU){ uop := uOpMultu ; io.signals.useHilo := True ; io.signals.multiCycle := True }
        is (FUN_DIV)  { uop := uOpDiv   ; io.signals.useHilo := True ; io.signals.multiCycle := True }
        is (FUN_DIVU) { uop := uOpDivu  ; io.signals.useHilo := True ; io.signals.multiCycle := True }
        is (FUN_SLT)  { uop := uOpSlt  }
        is (FUN_SLTU) { uop := uOpSltu }
        is (FUN_MOVN) { uop := uOpMovn }
        is (FUN_MOVZ) { uop := uOpMovz }
        default {
          io.signals.validInst := False
        }
      }
    }
    is (OP_COP0) {
      when (rs.msb === True) {// C0 Area
        switch (funct) {
          is (FUN_ERET) {
            uop := uOpEret
          }
          is (FUN_TLBP) {
            uop := uOpTlbp
          }
          is (FUN_TLBR) {
            uop := uOpTlbr
          }
          is (FUN_TLBWI) {
            uop := uOpTlbwi
          }
          is (FUN_WAIT) {
            uop := uOpSll
          }
          default {
            io.signals.validInst := False
          }
        }
      } otherwise {
        switch (rs) {
          is (RS_MFC0) { 
            uop := uOpMfc0 
          }
          is (RS_MTC0) { 
            uop := uOpMtc0
          }
          default {
            io.signals.validInst := False
          }
        }
      }
    }
    is (OP_REGIMM) {
      switch (rt) { // todo
        is (RT_BGEZ, RT_BGEZL)     { uop := uOpBgez   }
        is (RT_BGEZAL, RT_BGEZALL) { uop := uOpBgezal }
        is (RT_BLTZ, RT_BLTZL)     { uop := uOpBltz   } 
        is (RT_BLTZAL, RT_BLTZALL) { uop := uOpBltzal }
        default {
          io.signals.validInst := False
        }
      }
    }
    is (OP_SPEC2) {
      switch (funct) {
        is (FUN_MUL) { uop := uOpMul }
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
    is (OP_BEQ, OP_BEQL ) { uop := uOpBeq  }
    is (OP_BNE, OP_BNEL ) { uop := uOpBne  }
    is (OP_BGTZ, OP_BGTZL ) { uop := uOpBgtz }
    is (OP_BLEZ, OP_BLEZL ) { uop := uOpBlez }
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
    is (OP_PREF  ) { uop := uOpSll  }
    is (OP_CACHE ) { uop := uOpSll  }
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
    is (OP_BEQ, OP_BNE, OP_BGTZ, OP_BLEZ,
        OP_BEQL, OP_BNEL, OP_BGTZL, OP_BLEZL) {
      // io.signals.useImm    := True
      extImm    := brOffset
      io.signals.wrRegEn   := False
      io.signals.isBr      := True
    }
    is (OP_J) {
      io.signals.useImm    := True
      extImm    := jTarget
      io.signals.op1RdGeRf := False
      io.signals.op2RdGeRf := False
      io.signals.wrRegEn   := False
      io.signals.isBr      := True
    }
    is (OP_JAL) {
      io.signals.useImm    := True
      extImm               := jTarget
      io.signals.op1RdGeRf := False
      io.signals.op2RdGeRf := False
      io.signals.wrRegEn   := True
      io.signals.isBr      := True
    }
    is (OP_LB, OP_LBU, OP_LH, OP_LHU, OP_LW) {
      // io.signals.useImm    := True
      extImm    := sextImm
      io.signals.wrRegAddr  := rt
      io.signals.op2RdGeRf := False
    }
    is (OP_SB, OP_SH, OP_SW) {
      extImm    := sextImm
      io.signals.wrRegEn   := False
    }

    is (OP_REGIMM) {
      switch (rt) {
        is (RT_BGEZ, RT_BLTZ, RT_BLTZL, RT_BGEZL) {
          extImm               := brOffset
          io.signals.op2RdGeRf := False
          io.signals.wrRegEn   := False
          io.signals.isBr      := True
        }
        is (RT_BGEZAL, RT_BLTZAL, RT_BLTZALL, RT_BGEZALL) {
          extImm                := brOffset
          io.signals.op2RdGeRf  := False
          io.signals.wrRegAddr  := 31
          io.signals.isBr       := True
        }
      }
    }

    is (OP_PREF, OP_CACHE) {
      io.signals.op1RdGeRf := False
      io.signals.op2RdGeRf := False
      io.signals.wrRegEn   := False
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
          io.signals.isBr      := True
        }
        is (FUN_JALR) {
          io.signals.op2RdGeRf := False
          io.signals.wrRegAddr := 31
          io.signals.isBr      := True
        }
        is (FUN_SYSCALL, FUN_BREAK) {
          io.signals.wrRegEn   := False
          io.signals.op1RdGeRf := False
          io.signals.op2RdGeRf := False
          io.signals.isPriv    := True
        }
        is (FUN_MTHI, FUN_MTLO) {
          io.signals.wrRegEn   := False
          io.signals.op2RdGeRf := False
        }
      }
    }
    is (OP_SPEC2) {
      switch (funct) {
        is (FUN_MUL) {
          // TODO: MUL Implementation
          io.signals.op1RdGeRf := False
          io.signals.op2RdGeRf := False
          io.signals.wrRegEn   := False
        }
      }
    }
    is (OP_COP0) {
      when (rs.msb === True) {
        switch (funct) {
          is (FUN_ERET, FUN_TLBP, FUN_TLBR, FUN_TLBWI, FUN_WAIT) {
            io.signals.isPriv    := True
            io.signals.op1RdGeRf := False
            io.signals.op2RdGeRf := False
            io.signals.wrRegEn   := False
          }
        }
      } otherwise {
        switch (rs) {
          is (RS_MFC0) {
            io.signals.isPriv    := True
            io.signals.op1RdGeRf := False
            io.signals.op2RdGeRf := False
            io.signals.wrRegAddr := rt
          }
          is (RS_MTC0) {
            io.signals.isPriv    := True
            io.signals.op1Addr   := rt
            io.signals.op2RdGeRf := False
            io.signals.wrRegEn   := False
          }
        }
      }
    }
  }

  when (!io.signals.validInst) {
    io.signals.op1RdGeRf := False
    io.signals.op2RdGeRf := False
    io.signals.wrRegEn   := False
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