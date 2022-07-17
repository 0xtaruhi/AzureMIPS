package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.idu.ReadRfSignals
import azuremips.core.ExceptionCode._

class ExecutedSignals extends Bundle {
  val wrRegEn   = Bool()
  val wrRegAddr = UInt(5 bits)
  val wrMemEn   = Bool()
  val rdMemEn   = Bool()
  val wrMemMask = UInt(4 bits)
  val wrHi      = Bool()
  val wrLo      = Bool()
  val memVAddr  = UInt(32 bits)
  val wrData    = UInt(32 bits)
  val exptEn    = Bool()
  val exptCode  = UInt(exptCodeWidth bits)
}

class Execute extends Component {
  val io = new Bundle {
    val readrfSignals   = in Vec(new ReadRfSignals, 2)
    val executedSignals = out Vec(new ExecutedSignals, 2)
  }

  (io.readrfSignals zip io.executedSignals).foreach {
    case (readrf, execute) => {
      switch (readrf.uop) {
        is (uOpAdd, uOpAddu) {
          execute.wrData := readrf.op1Data + readrf.op2Data
        }
        is (uOpSub, uOpSubu) {
          execute.wrData := readrf.op1Data - readrf.op2Data
        }
        is (uOpSlt) {
          execute.wrData := Mux(S(readrf.op1Data) < S(readrf.op2Data), U(1), U(0)).resized
        }
        is (uOpSltu) {
          execute.wrData := Mux(readrf.op1Data < readrf.op2Data, U(1), U(0)).resized
        }
        is (uOpAnd) {
          execute.wrData := readrf.op1Data & readrf.op2Data
        }
        is (uOpOr) {
          execute.wrData := readrf.op1Data | readrf.op2Data
        }
        is (uOpXor) {
          execute.wrData := readrf.op1Data ^ readrf.op2Data
        }
        is (uOpLui) {
          execute.wrData := readrf.imm |<< 16
        }
        is (uOpNor) {
          execute.wrData := ~(readrf.op1Data | readrf.op2Data)
        }
        is (uOpSll) {
          execute.wrData := readrf.op2Data |<< readrf.imm(4 downto 0)
        }
        is (uOpSllv) {
          execute.wrData := readrf.op2Data |<< readrf.op1Data(4 downto 0)
        }
        is (uOpSrav) {
          execute.wrData := U(S(readrf.op2Data) |>> readrf.op1Data(4 downto 0))
        }
        is (uOpSra) {
          execute.wrData := U(S(readrf.op2Data) |>> readrf.imm(4 downto 0))
        }
        is (uOpSrl) {
          execute.wrData := readrf.op2Data |>> readrf.imm(4 downto 0)
        }
        is (uOpSrlv) {
          execute.wrData := readrf.op2Data |>> readrf.op1Data(4 downto 0)
        }
        is (uOpJal, uOpJalr, uOpBgezal, uOpBltzal) {
          execute.wrData := readrf.pc + 8
        }
        // form hi/lo or to hi/lo
        is (uOpMfhi, uOpMflo, uOpMthi, uOpMtlo) {
          execute.wrData := readrf.op1Data
        }
        // to memory
        is (uOpSb) {
          execute.wrData := readrf.op2Data & U(0x07)
        }
        is (uOpSh) {
          execute.wrData := readrf.op2Data & U(0x0F)
        }
        is (uOpSw) {
          execute.wrData := readrf.op2Data
        }
        default {
          execute.wrData := U(0)
        }
      }
      execute.wrRegAddr := readrf.wrRegAddr
      execute.memVAddr := readrf.imm + readrf.op1Data

      // switch (readrf.uop) {
      //   is (uOpAdd,  uOpAddu, uOpSub,    uOpSubu, uOpSlt, uOpSltu, 
      //       uOpAnd,  uOpLui,  uOpNor,    uOpOr,   uOpXor, 
      //       uOpSllv, uOpSll,  uOpSrav,   uOpSra,  uOpSrl, uOpSrlv,
      //       uOpJal,  uOpJalr, uOpBgezal, uOpBltzal,
      //       uOpMfhi, uOpMflo, 
      //       uOpLb,   uOpLbu,  uOpLh, uOpLhu, uOpLw) {
      //     execute.wrRegEn := True
      //   }
      //   default { execute.wrRegEn := False }
      // }
      execute.wrRegEn := readrf.wrRegEn

      // execute.wrMemEn := Mux(readrf.isStore, True, False)
      // execute.rdMemEn := Mux(readrf.isLoad,  True, False)
      switch (readrf.uop) {
        is (uOpLb, uOpLbu, uOpLh, uOpLhu, uOpLw) {
          execute.wrMemEn := False
          execute.rdMemEn := True
        }
        is (uOpSb, uOpSh, uOpSw) {
          execute.wrMemEn := True
          execute.rdMemEn := False
        }
        default {
          execute.wrMemEn := False
          execute.rdMemEn := False
        }
      }
      switch (readrf.uop) {
        is (uOpSb) {
          execute.wrMemMask := U"0001"
        }
        is (uOpSh) {
          execute.wrMemMask := U"0011"
        }
        is (uOpSw) {
          execute.wrMemMask := U"1111"
        }
        default { execute.wrMemMask := 0 }
      }

      execute.wrHi := Mux(readrf.uop === uOpMthi, True, False)
      execute.wrLo := Mux(readrf.uop === uOpMtlo, True, False)

      execute.exptEn := False
      execute.exptCode := 0
    }
  }
}

object GenExcute {
  def main(args: Array[String]) {
    SpinalVerilog(new Execute)
  }
}