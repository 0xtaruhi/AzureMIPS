package azuremips.core.cp0

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._

case class ExptInfo() extends Bundle {
  val exptValid = Bool()
  val exptCode  = UInt(exptCodeWidth bits)
  val eret      = Bool()

  def emptyExptInfo = {
    val e = ExptInfo()
    e.exptValid := False
    e.exptCode  := 0
    e.eret      := False
    e
  }
}

case class ExptReq() extends Bundle with IMasterSlave {
  val exptInfo   = ExptInfo()
  val exptPc     = UInt(32 bits)
  val memVAddr   = UInt(32 bits)
  val inBD       = Bool()
  // val redirectEn = Bool()
  // val redirectPc = UInt(32 bits)

  override def asMaster {
    out(exptInfo, exptPc, inBD, memVAddr)
    // in(redirectEn, redirectPc)
  }
}

class Cp0ReadPort extends Bundle with IMasterSlave {
  val addr = UInt(5 bits)
  val sel  = UInt(3 bits)
  val data = UInt(32 bits)

  override def asMaster {
    out(addr, sel)
    in(data)
  }
}

class Cp0WritePort extends Bundle with IMasterSlave {
  val addr = UInt(5 bits)
  val sel  = UInt(3 bits)
  val data = UInt(32 bits)
  val wen  = Bool()
  val pc   = UInt(32 bits)
  
  override def asMaster {
    out(addr, sel, data, wen, pc)
  }
}

class Cp0 extends Component {
  val io = new Bundle {
    val read       = slave(new Cp0ReadPort)
    val write      = slave(new Cp0WritePort)
    val exptReq    = slave(ExptReq())
    val redirectEn = out Bool()
    val redirectPc = out UInt(32 bits) 
    val hwInterrupt = in UInt(6 bits)
    val hwIntMemAvail = in Bool()
  }
  io.redirectEn := False
  io.redirectPc := 0
  io.read.data.setAsReg.init(0)

  val badVAddr = Reg(UInt(32 bits)) init (0)
  val count    = UInt(32 bits)
  val status   = Reg(UInt(32 bits)) init (U(32 bits, 22 -> true, default -> false))
  val cause    = Reg(UInt(32 bits)) init (0)
  val epc      = Reg(UInt(32 bits)) init (0)
  val compare  = Reg(UInt(32 bits)) init (0)

  val _counter = Reg(UInt(33 bits)) init (0)
  count := _counter(32 downto 1)
  _counter := _counter + 1

  val exl      = status(1)
  val bd       = cause(31)
  // val causeIp  = cause(15 downto 8)
  // val statusIp = status(15 downto 8)
  // val interrupt = (causeIp & statusIp).orR
  val causeExcCode = cause(6 downto 2)
  val statusIe = status(0)
  val causeTI = cause(30)

  val statusWrMask = U(32 bits, (15 downto 8) -> true, (1 downto 0) -> true, default -> false)
  val causeWrMask  = U(32 bits, (9 downto 8) -> true, default -> false)
  val statusWrData = io.write.data & statusWrMask | status & ~statusWrMask
  val causeWrData  = io.write.data & causeWrMask | cause & ~causeWrMask

  when (exl === False && io.exptReq.exptInfo.exptValid && !io.exptReq.exptInfo.eret) {
    exl := True
    io.redirectEn := True
    io.redirectPc := U"32'hbfc00380"
    epc := io.exptReq.exptPc
    bd  := io.exptReq.inBD

    switch (io.exptReq.exptInfo.exptCode) {
      is (EXC_ADEL, EXC_ADEL_FI) {
        badVAddr      := io.exptReq.memVAddr
        causeExcCode  := 0x04
      }
      is (EXC_ADES) {
        badVAddr      := io.exptReq.memVAddr
        causeExcCode  := 0x05
      }
      is (EXC_OVF) {
        causeExcCode  := 0x0c
      }
      is (EXC_SYSCALL) {
        causeExcCode  := 0x08
      }
      is (EXC_BREAK) {
        causeExcCode  := 0x09
      }
      is (EXC_RESERVED) {
        causeExcCode  := 0x0a
      }
    }
  }


  when (io.exptReq.exptInfo.eret) {
    when (epc(1 downto 0) === U"00") {
      exl := False
      io.redirectEn := True
      io.redirectPc := epc
    } otherwise {
      exl := True
      causeExcCode  := 0x04
      io.redirectEn := True
      io.redirectPc := U"32'hbfc00380"
      badVAddr      := epc
    }
  }

  switch (io.read.addr) {
    is (8) {
      io.read.data := badVAddr
    }
    is (9) {
      io.read.data := count
    }
    is (11) {
      io.read.data := compare
    }
    is (12) {
      io.read.data := status
    }
    is (13) {
      io.read.data := cause
    }
    is (14) {
      io.read.data := epc
    }
  }

  when (io.read.addr === io.write.addr && io.write.wen) {
    switch (io.read.addr) {
      is (12) {
        io.read.data := statusWrData
      }
      is (13) {
        io.read.data := causeWrData
      }
    }
  }

  when (io.write.wen) {
    switch (io.write.addr) {
      is (9) {
        _counter(32 downto 1) := io.write.data
      }
      is (11) {
        compare := io.write.data
      }
      is (12) {
        status := statusWrData
      }
      is (13) {
        cause := causeWrData
      }
      is (14) {
        epc := io.write.data
      }
    }
  }
  def statusIMRange = (15 downto 8)
  def causeIPRange  = (15 downto 8)
  val writeStatus = (io.write.addr === U(12) && (io.write.sel === U(0)))
  val writeCause  = (io.write.addr === U(13) && (io.write.sel === U(0)))

  when (exl === False && statusIe === True) {
    val swInterrupt = False
    when (writeCause) {
      swInterrupt.setWhen((causeWrData(causeIPRange) & status(statusIMRange)).orR)
    }
    when (writeStatus) {
      swInterrupt.setWhen((statusWrData(statusIMRange) & cause(causeIPRange)).orR)
    }
    val causeIP  = cause(causeIPRange)
    val statusIM = status(statusIMRange)
    val hwInterrupt = False
    when (((count === compare && compare =/= 0) || io.hwInterrupt(5)) && statusIM(7)) {
      hwInterrupt := True
      causeIP(7) := True 
    }
    when (io.hwInterrupt(4) && statusIM(6)) {
      hwInterrupt := True
      causeIP(6) := True
    }
    when (io.hwInterrupt(3) && statusIM(5)) {
      hwInterrupt := True
      causeIP(5) := True
    }
    when (io.hwInterrupt(2) && statusIM(4)) {
      hwInterrupt := True
      causeIP(4) := True
    }
    when (io.hwInterrupt(1) && statusIM(3)) {
      hwInterrupt := True
      causeIP(3) := True
    }
    when (io.hwInterrupt(0) && statusIM(2)) {
      hwInterrupt := True
      causeIP(2) := True
    }
    when (swInterrupt || (hwInterrupt && io.hwIntMemAvail)) {
      exl := True
      causeExcCode := 0x00
      io.redirectEn := True
      io.redirectPc := U"32'hbfc00380"
      epc := io.write.pc
    }
  }
}

object GenCp0Verilog extends App {
  SpinalVerilog(new Cp0)
}