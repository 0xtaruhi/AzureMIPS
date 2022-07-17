package azuremips.core.cp0

import spinal.core._
import spinal.lib._

import azuremips.core._

import azuremips.core.ExceptionCode._

class ExptInfo extends Bundle with IMasterSlave {
  val exptValid = Bool()
  val exptCode  = UInt(exptCodeWidth bits)
  val pc        = UInt(32 bits)
  val inBD      = Bool() // branch delay slot

  override def asMaster {
    out(exptValid, exptCode, pc, inBD)
  }
}

class Cp0 extends Component {
  val io = new Bundle {
    val rdAddr = in UInt(5 bits)
    val rdSel  = in UInt(3 bits)
    val rdData = out UInt(32 bits) setAsReg

    val wrAddr = in UInt(5 bits)
    val wrSel  = in UInt(3 bits)
    val wrData = in UInt(32 bits)
    val writeMask = in UInt(32 bits)

    val expt   = slave(new ExptInfo)
    val eret   = in Bool()
  }

  val badVAddr = Reg(UInt(32 bits)) init(0)
  val countReg = RegInit(U(0, 33 bits))
  val count    = UInt(32 bits)
  val status   = Reg(UInt(32 bits)) init(U"00000000010000000000000000000000")
  val cause    = Reg(UInt(32 bits)) init(0)
  val epc      = Reg(UInt(32 bits)) init(0)
  
  countReg := countReg + 1
  count := countReg(32 downto 1)

  val statusHardZero = U"00000000010000001111111100000011"
  val causeHardZero  = U"11000000000000001111111101111100"

  io.rdData := 0
  when (io.rdSel === U(0)) {
    switch (io.rdAddr) {
      is (U(8)) {
        io.rdData := badVAddr
      }
      is (U(9)) {
        io.rdData := count
      }
      is (U(12)) {
        io.rdData := status
      }
      is (U(13)) {
        io.rdData := cause
      }
      is (U(14)) {
        io.rdData := epc
      }
    }
  }

  val dataAfterMask = io.wrData & io.writeMask

  when (io.wrSel === U(0)) {
    switch (io.wrAddr) {
      is (U(8)) {
        badVAddr := dataAfterMask
      }
      is (U(9)) {
        countReg := dataAfterMask << 1
      }
      is (U(12)) {
        status := dataAfterMask & statusHardZero
      }
      is (U(13)) {
        cause := dataAfterMask & causeHardZero
      }
      is (U(14)) {
        epc := dataAfterMask
      }
    }
  }

  val statusEXL = status(1)
  val causeBD   = cause(31)
  val causeExcCode = cause(6 downto 2)
  when (io.expt.exptValid) {
    when (statusEXL === False) {
      when (io.expt.inBD) {
        epc := io.expt.pc - 4
        causeBD := True
      } otherwise {
        epc := io.expt.pc
        causeBD := False
      }
      switch (io.expt.exptCode) {
        is (EXC_INTERCEPT) {
          causeExcCode := 0
        }
        is (EXC_ADEL) {
          causeExcCode := 4
        }
        is (EXC_ADES) {
          causeExcCode := 5
        }
        is (EXC_SYSCALL) {
          causeExcCode := 8
        }
        is (EXC_BREAK) {
          causeExcCode := 9
        }
        is (EXC_RESERVED) {
          causeExcCode := 10
        }
      }
    }
    statusEXL := True
  }
  when (io.eret) {
    statusEXL := False
  }

}

object GenCp0Verilog extends App {
  SpinalVerilog(new Cp0)
}