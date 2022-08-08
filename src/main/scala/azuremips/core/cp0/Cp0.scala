package azuremips.core.cp0

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._
import azuremips.core.except.PcArbiter
import azuremips.core.mmu.TlbConfig
import azuremips.core.mmu.{WriteTlbPort, ReadTlbPort, ProbeTlbReq}

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

class Cp0 extends Component with TlbConfig {
  val io = new Bundle {
    val read       = slave(new Cp0ReadPort)
    val write      = slave(new Cp0WritePort)
    val exptReq    = slave(ExptReq())
    val redirectEn = out Bool()
    val redirectPc = out UInt(32 bits) 
    val hwInterrupt = in UInt(6 bits)
    val hwIntMemAvail = in Bool()
    val hwIntTrig   = out Bool()
    // Tlb
    val asid       = out UInt(8 bits)
    val tlbWen     = in Bool()
    val tlbWrite   = master(WriteTlbPort())
    val tlbRen     = in Bool()
    val tlbRead    = master(ReadTlbPort())
    val tlbProbeEn = in Bool()
    val tlbProbe   = master(ProbeTlbReq())
  }

  io.redirectEn := False
  io.redirectPc := 0
  io.read.data.setAsReg.init(0)

  val index    = Reg(UInt(32 bits)) init (0)
  // val random   = Reg(UInt(32 bits)) init (0)
  val entryLo0 = Reg(UInt(32 bits)) init (0)
  val entryLo1 = Reg(UInt(32 bits)) init (0)
  // val context  = Reg(UInt(32 bits)) init (0)
  val pageMask = Reg(UInt(32 bits)) init (0)
  // val wired    = Reg(UInt(32 bits)) init (0)
  val badVAddr = Reg(UInt(32 bits)) init (0)
  val count    = UInt(32 bits)
  val entryHi  = Reg(UInt(32 bits)) init (0)
  val compare  = Reg(UInt(32 bits)) init (0)
  val status   = Reg(UInt(32 bits)) init (U(32 bits, 22 -> true, default -> false))
  val cause    = Reg(UInt(32 bits)) init (0)
  val epc      = Reg(UInt(32 bits)) init (0)
  // val pi       = Reg(UInt(32 bits)) init (0)
  // val eBase    = Reg(UInt(32 bits)) init (0)
  val config   = Reg(UInt(32 bits)) init (U(32 bits, 31 -> true, 7 -> true, 1 -> true, default -> false))
  val config1  = Reg(UInt(32 bits)) init (0)
  // import azuremips.core.cache.{ICacheConfig, DCacheConfig}
  // val icacheWayNum = ICacheConfig().wayNum
  // val icacheSetNum = ICacheConfig().setNum
  // val dcacheWayNum = DCacheConfig().wayNum
  // val dcacheSetNum = DCacheConfig().setNum
  // val config1  = Reg(UInt(32 bits)) init (U(32 bits, 31 -> true /* TODO: cache config */))
  // val config1  = Reg(UInt(32 bits)) init (U(
  //   31 -> false, // Config2 redigster is not implemented
  //   (30 downto 25) -> log2Up(tlbSize),
  //   (24 downto 22) -> { if (icacheSetNum == 32) 7 else { log2Up(icacheSetNum) - 6 } },
  //   (21 downto 19) -> (log2Up(ICacheConfig().cacheLineWidth) + 1),
  //   (18 downto 16) -> (icacheWayNum - 1),
  //   (15 downto 13) -> { if (dcacheSetNum == 32) 7 else { log2Up(dcacheSetNum) - 6 } },
  //   (12 downto 10) -> (log2Up(DCacheConfig().cacheLineWidth) + 1),
  //   (9 downto 7) -> (dcacheWayNum - 1),
  //   (6 downto 2) -> 0,
  //   (1 downto 0) -> 0
  // ))
  // val tagLo    = Reg(UInt(32 bits)) init (0)
  // val tagHi    = Reg(UInt(32 bits)) init (0)

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

  val indexWrMask    = U(32 bits, (4 downto 0) -> true, default -> false)
  val entryLo0WrMask = U(32 bits, (25 downto 0) -> true, default -> false)
  val entryLo1WrMask = U(32 bits, (25 downto 0) -> true, default -> false)
  val entryHiWrMask  = U(32 bits, (31 downto 13) -> true, (7 downto 0) -> true, default -> false)
  val pageMaskWrMask = U(32 bits, (24 downto 13) -> true, default -> false)
  val statusWrMask   = U(32 bits, (15 downto 8) -> true, (1 downto 0) -> true, default -> false)
  val causeWrMask    = U(32 bits, (9 downto 8) -> true, default -> false)
  val configWrMask   = U(32 bits, (2 downto 0) -> true, default -> false)
  val indexWrData    = io.write.data & indexWrMask | index & ~indexWrMask
  val entryLo0WrData = io.write.data & entryLo0WrMask | entryLo0 & ~entryLo0WrMask
  val entryLo1WrData = io.write.data & entryLo1WrMask | entryLo1 & ~entryLo1WrMask
  val entryHiWrData  = io.write.data & entryHiWrMask | entryHi & ~entryHiWrMask
  val pageMaskWrData = io.write.data & pageMaskWrMask | pageMask & ~pageMaskWrMask
  val statusWrData   = io.write.data & statusWrMask | status & ~statusWrMask
  val causeWrData    = io.write.data & causeWrMask | cause & ~causeWrMask
  val configWrData   = io.write.data & configWrMask | config & ~configWrMask

  val pcArbiter = PcArbiter()
  pcArbiter.io.interrupt := False
  pcArbiter.io.exptCode  := io.exptReq.exptInfo.exptCode
  pcArbiter.io.bev       := status(22)
  pcArbiter.io.exl       := exl
  pcArbiter.io.iv        := cause(23)

  when (exl === False && io.exptReq.exptInfo.exptValid && !io.exptReq.exptInfo.eret) {
    exl := True
    io.redirectEn := True
    // io.redirectPc := U"32'hbfc00380"
    io.redirectPc := pcArbiter.io.redirectPc
    // epc := io.exptReq.exptPc
    epc := Mux(io.exptReq.exptInfo.exptCode === EXC_ADEL_FI, 
               io.exptReq.memVAddr, io.exptReq.exptPc)
    bd  := io.exptReq.inBD

    switch (io.exptReq.exptInfo.exptCode) {
      is (EXC_TLBMOD) {
        causeExcCode  := 0x01
      }
      is (EXC_TLBREFILL_L, EXC_TLBINVALID_L) {
        causeExcCode  := 0x02
      }
      is (EXC_TLBREFILL_S, EXC_TLBINVALID_S) {
        causeExcCode  := 0x03
      }
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
    is (0) {
      io.read.data := index
    }
    is (2) {
      io.read.data := entryLo0
    }
    is (3) {
      io.read.data := entryLo1
    }
    is (8) {
      io.read.data := badVAddr
    }
    is (9) {
      io.read.data := count
    }
    is (10) {
      io.read.data := entryHi
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
    is (16) {
      io.read.data := Mux(io.read.sel === 0, config, config1)
    }
  }

  when (io.read.addr === io.write.addr && io.write.wen) {
    switch (io.read.addr) {
      is (0) {
        io.read.data := indexWrData
      }
      is (2) {
        io.read.data := entryLo0WrData
      }
      is (3) {
        io.read.data := entryLo1WrData
      }
      // is (9) {
      //   io.read.data := io.write.data
      // }
      is (10) {
        io.read.data := entryHiWrData
      }
      // is (11) {
      //   io.read.data := io.write.data
      // }
      is (12) {
        io.read.data := statusWrData
      }
      is (13) {
        io.read.data := causeWrData
      }
      is (16) {
        io.read.data := configWrData
      }
    }
  }

  when (io.write.wen) {
    switch (io.write.addr) {
      is (0) {
        index := indexWrData
      }
      is (2) {
        entryLo0 := entryLo0WrData
      }
      is (3) {
        entryLo1 := entryLo1WrData
      }
      is (9) {
        _counter(32 downto 1) := io.write.data
      }
      is (10) {
        entryHi := entryHiWrData
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
      is (16) {
        config := configWrData
      }
    }
  }
  def statusIMRange = (15 downto 8)
  def causeIPRange  = (15 downto 8)
  val writeStatus = (io.write.addr === U(12) && (io.write.sel === U(0)))
  val writeCause  = (io.write.addr === U(13) && (io.write.sel === U(0)))

  val statusIM = status(statusIMRange)
  val causeIP  = cause(causeIPRange)
  causeIP(7) := io.hwInterrupt(5) && statusIM(7) || (count === compare && compare =/= 0)
  causeIP(6) := io.hwInterrupt(4) && statusIM(6)
  causeIP(5) := io.hwInterrupt(3) && statusIM(5)
  causeIP(4) := io.hwInterrupt(2) && statusIM(4)
  causeIP(3) := io.hwInterrupt(1) && statusIM(3)
  causeIP(2) := io.hwInterrupt(0) && statusIM(2)

  io.hwIntTrig := False
  when (exl === False && statusIe === True) {
    val swInterrupt = False
    when (writeCause) {
      swInterrupt.setWhen((causeWrData(causeIPRange) & status(statusIMRange)).orR)
    }
    when (writeStatus) {
      swInterrupt.setWhen((statusWrData(statusIMRange) & cause(causeIPRange)).orR)
    }
    val hwInterrupt = causeIP(7 downto 2).orR
    io.hwIntTrig := hwInterrupt && io.hwIntMemAvail
    when (swInterrupt || (hwInterrupt && io.hwIntMemAvail)) {
      exl := True
      causeExcCode := 0x00
      io.redirectEn := True
      io.redirectPc := U"32'hbfc00380"
      // epc := io.write.pc
    }
    when (swInterrupt) {
      epc := io.write.pc + 4
    }
    when (hwInterrupt && io.hwIntMemAvail) {
      epc := io.write.pc
    }
  }

  // TLB
  io.asid := entryHi(7 downto 0)
  // Write
  io.tlbWrite.index    := index((log2Up(tlbSize) - 1) downto 0)
  io.tlbWrite.entryHi  := entryHi
  io.tlbWrite.pageMask := pageMask
  io.tlbWrite.entryLo0 := entryLo0
  io.tlbWrite.entryLo1 := entryLo1
  io.tlbWrite.wen      := io.tlbWen
  // Read
  io.tlbRead.index := index((log2Up(tlbSize) - 1) downto 0)
  when (io.tlbRen) {
    entryHi  := io.tlbRead.entryHi
    entryLo0 := io.tlbRead.entryLo0
    entryLo1 := io.tlbRead.entryLo1
    pageMask := io.tlbRead.pageMask
  }
  // Probe
  io.tlbProbe.vpn2 := entryHi(31 downto 13)
  when (io.tlbProbeEn) {
    when (!io.tlbProbe.found) {
      index.msb := True
    } otherwise {
      index := io.tlbProbe.index.resized
    }
  }
}

object GenCp0Verilog extends App {
  SpinalVerilog(new Cp0)
}