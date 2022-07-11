package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._

class Alu extends Component {
  val io = new Bundle {
    val op1    = in UInt(32 bits)
    val op2    = in UInt(32 bits)
    val pc     = in UInt(32 bits)
    val offset = in UInt(16 bits)
    val uop    = in UInt(AzureConsts.uopWidth bits)
    val excIOFValid = in Bool()
    val result = out UInt(32 bits)
    val taken  = out Bool()
    val target = out UInt(32 bits)
    val excIOF = out Bool()
  }
  import Uops._
  io.result := 0
  io.taken  := False
  io.target := U(0)

  def ofsExt(ofs: UInt, width: Int = 32) = {
    S(ofs << 2).resize(width).asUInt
  }

  switch (io.uop) {
    is (ALU_ADD) {
      io.result := io.op1 + io.op2
    }
    is (ALU_SUB) {
      io.result := io.op1 - io.op2
    }
    is (ALU_AND) {
      io.result := io.op1 & io.op2
    }
    is (ALU_OR) {
      io.result := io.op1 | io.op2
    }
    is (ALU_XOR) {
      io.result := io.op1 ^ io.op2
    }
    is (ALU_SLL) {
      io.result := io.op1 |<< io.op2
    }
    is (ALU_SRL) {
      io.result := io.op1 |>> io.op2
    }
    is (ALU_SRA) {
      io.result := U(S(io.op1) >> io.op2)
    }
    is (ALU_SLT) {
      io.result := Mux(S(io.op1) < S(io.op2), U(1, 32 bits), U(0, 32 bits))
    }
    is (ALU_SLTU) {
      io.result := Mux(io.op1 < io.op2, U(1, 32 bits), U(0, 32 bits))
    }
    is (ALU_LUI) {
      io.result := io.op1 |<< 16
    }
    is (ALU_NOR) {
      io.result := ~(io.op1 | io.op2)
    }
    is (ALU_JAL) {
      io.result := io.pc + 8
    }
  }

  io.excIOF := False
  when (io.excIOFValid) {
    switch (io.uop) {
      is (ALU_ADD) {
        io.excIOF := (io.result < io.op1) || (io.result < io.op2)
      }
      is (ALU_SUB) {
        io.excIOF := (io.op1 < io.op2)
      }
      default {
        io.excIOF := False
      }
    }
  }

}
