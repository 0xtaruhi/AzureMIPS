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

  val pcArbiter = PcArbiter()


  io.redirectEn := False
  io.redirectPc := 0
  io.read.data := 0

  val index    = Reg(UInt(32 bits)) init (0)
  // val random   = Reg(UInt(32 bits)) init (0)
  val entryLo0 = Reg(UInt(32 bits)) init (0)
  val entryLo1 = Reg(UInt(32 bits)) init (0)
  val context  = Reg(UInt(32 bits)) init (0)
  val pageMask = Reg(UInt(32 bits)) init (0)
  val wired    = Reg(UInt(32 bits)) init (0)
  val badVAddr = Reg(UInt(32 bits)) init (0)
  val count    = UInt(32 bits)
  val entryHi  = Reg(UInt(32 bits)) init (0)
  val compare  = Reg(UInt(32 bits)) init (0)
  val status   = Reg(UInt(32 bits)) init (U(32 bits, 22 -> true, default -> false))
  val cause    = Reg(UInt(32 bits)) init (0)
  val epc      = Reg(UInt(32 bits)) init (0)
  val prId     = U"32'h00018000"
  val eBase    = Reg(UInt(32 bits)) init (U(32 bits, 31 -> true, default -> false))
  val config   = Reg(UInt(32 bits)) init (U(32 bits, 31 -> true, 7 -> true, 1 -> true, default -> false))
  import azuremips.core.cache.{ICacheConfig, DCacheConfig}
  val icacheWayNum = ICacheConfig().wayNum
  val icacheSetNum = ICacheConfig().setNum
  val dcacheWayNum = DCacheConfig().wayNum
  val dcacheSetNum = DCacheConfig().setNum
  val config1      = UInt(32 bits)
  config1(31)           := False // There's no Config2 register
  config1(30 downto 25) := log2Up(tlbSize)
  config1(24 downto 22) := { if (icacheSetNum == 32) 7 else { log2Up(icacheSetNum) - 6} }
  config1(21 downto 19) := (log2Up(ICacheConfig().cacheLineWidth) + 1)
  config1(18 downto 16) := (icacheWayNum - 1)
  config1(15 downto 13) := { if (dcacheSetNum == 32) 7 else { log2Up(dcacheSetNum) - 6} }
  config1(12 downto 10) := (log2Up(DCacheConfig().cacheLineWidth) + 1)
  config1(9 downto 7) := (dcacheWayNum - 1)
  config1(6 downto 0) := 0
  val tagLo    = Reg(UInt(32 bits)) init (0)
  val tagHi    = Reg(UInt(32 bits)) init (0)

  val _counter = Reg(UInt(33 bits)) init (0)
  count := _counter(32 downto 1)
  _counter := _counter + 1

  val exl      = status(1)
  val bd       = cause(31)
  val causeExcCode = cause(6 downto 2)
  val statusIe = status(0)
  val causeTI = cause(30)
  val causeCE = cause(29 downto 28)

  val indexWrMask    = U(32 bits, (4 downto 0) -> true, default -> false)
  val entryLo0WrMask = U(32 bits, (25 downto 0) -> true, default -> false)
  val entryLo1WrMask = U(32 bits, (25 downto 0) -> true, default -> false)
  val contextWrMask  = U(32 bits, (31 downto 23) -> true, default -> false)
  val entryHiWrMask  = U(32 bits, (31 downto 13) -> true, (7 downto 0) -> true, default -> false)
  val pageMaskWrMask = U(32 bits, (28 downto 11) -> true, default -> false)
  val wiredWrMask    = U(32 bits, ((log2Up(tlbSize) - 1) downto 0) -> true, default -> false)
  val statusWrMask   = U(32 bits, (15 downto 8) -> true, 22 -> true, (1 downto 0) -> true, default -> false)
  val causeWrMask    = U(32 bits, (9 downto 8) -> true, default -> false)
  val eBaseWrMask    = U(32 bits, (29 downto 12) -> true, default -> false)
  val configWrMask   = U(32 bits, (2 downto 0) -> true, default -> false)
  val indexWrData    = io.write.data & indexWrMask | index & ~indexWrMask
  val entryLo0WrData = io.write.data & entryLo0WrMask | entryLo0 & ~entryLo0WrMask
  val entryLo1WrData = io.write.data & entryLo1WrMask | entryLo1 & ~entryLo1WrMask
  val contextWrData  = io.write.data & contextWrMask | context & ~contextWrMask
  val entryHiWrData  = io.write.data & entryHiWrMask | entryHi & ~entryHiWrMask
  val pageMaskWrData = io.write.data & pageMaskWrMask | pageMask & ~pageMaskWrMask
  val wiredWrData    = io.write.data & wiredWrMask | wired & ~wiredWrMask
  val statusWrData   = io.write.data & statusWrMask | status & ~statusWrMask
  val causeWrData    = io.write.data & causeWrMask | cause & ~causeWrMask
  val eBaseWrData    = io.write.data & eBaseWrMask | eBase & ~eBaseWrMask
  val configWrData   = io.write.data & configWrMask | config & ~configWrMask

  def statusIMRange = (15 downto 8)
  def causeIPRange  = (15 downto 8)
  val writeStatus = (io.write.addr === U(12) && (io.write.sel === U(0)))
  val writeCause  = (io.write.addr === U(13) && (io.write.sel === U(0)))

  val statusIM = status(statusIMRange)
  val causeIP  = cause(causeIPRange)

  val timeInterrupt = RegInit(False)

  val vpn2 = entryHi(31 downto 13)

  pcArbiter.io.exptCode  := io.exptReq.exptInfo.exptCode
  pcArbiter.io.bev       := status(22)
  pcArbiter.io.exl       := exl
  pcArbiter.io.iv        := cause(23)
  pcArbiter.io.eBase     := eBase

  when (exl === False && io.exptReq.exptInfo.exptValid && !io.exptReq.exptInfo.eret) {
    exl := True
    io.redirectEn := True
    io.redirectPc := pcArbiter.io.redirectPc
    epc := Mux(io.exptReq.exptInfo.exptCode === EXC_ADEL_FI, 
               io.exptReq.memVAddr, io.exptReq.exptPc)
    bd  := io.exptReq.inBD

    switch (io.exptReq.exptInfo.exptCode) {
      is (EXC_TLBMOD) {
        causeExcCode  := 0x01
        badVAddr      := io.exptReq.memVAddr
        vpn2          := io.exptReq.memVAddr(31 downto 13)
      }
      is (EXC_TLBREFILL_L, EXC_TLBINVALID_L) {
        causeExcCode  := 0x02
        badVAddr      := io.exptReq.memVAddr
        vpn2          := io.exptReq.memVAddr(31 downto 13)
      }
      is (EXC_TLBREFILL_S, EXC_TLBINVALID_S) {
        causeExcCode  := 0x03
        badVAddr      := io.exptReq.memVAddr
        vpn2          := io.exptReq.memVAddr(31 downto 13)
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
      is (EXC_CP1_UNUSABLE) {
        causeExcCode  := 0x0b
        causeCE       := 0x01
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
      io.redirectPc := pcArbiter.io.redirectPc
      pcArbiter.io.exptCode := EXC_ADES
      badVAddr      := epc
    }
  }

  switch (io.read.addr) {
    is (0) { io.read.data := index }
    is (2) { io.read.data := entryLo0 }
    is (3) { io.read.data := entryLo1 }
    is (4) { io.read.data := context }
    is (5) { io.read.data := pageMask }
    is (6) { io.read.data := wired }
    is (8) { io.read.data := badVAddr }
    is (9) { io.read.data := count }
    is (10) { io.read.data := entryHi }
    is (11) { io.read.data := compare }
    is (12) { io.read.data := status }
    is (13) { io.read.data := cause }
    is (14) { io.read.data := epc }
    is (15) {
      switch (io.read.sel) {
        is (0) { io.read.data := prId }
        is (1) { io.read.data := eBase }
      }
    }
    is (16) { 
      switch (io.read.sel) {
        is (0) { io.read.data := config }
        is (1) { io.read.data := config1 }
      }
    }
    is (28) { io.read.data := tagLo }
    is (29) { io.read.data := tagHi }
  }

  when (io.write.wen) {
    switch (io.write.addr) {
      is (0) { index := indexWrData }
      is (2) { entryLo0 := entryLo0WrData }
      is (3) { entryLo1 := entryLo1WrData }
      is (4) { context := contextWrData }
      is (5) { pageMask := pageMaskWrData }
      is (6) { wired := wiredWrData }
      is (9) { _counter(32 downto 1) := io.write.data }
      is (10) { entryHi := entryHiWrData }
      is (11) { compare := io.write.data ; timeInterrupt := False}
      is (12) { status := statusWrData }
      is (13) { cause := causeWrData }
      is (14) { epc := io.write.data }
      is (15) { when (io.read.sel === 1) { eBase := eBaseWrData } }
      is (16) { when (io.read.sel === 0) { config := configWrData } // when (io.read.sel === 1) { config1 := configWrData } conifg1 is readonly
      is (28) { tagLo := io.write.data }
      is (29) { tagHi := io.write.data }
      }
    }
  }

  when (compare === count && compare =/= 0) {
    timeInterrupt := True
  }

  causeIP(7) := io.hwInterrupt(5) && statusIM(7) || timeInterrupt
  causeIP(6) := io.hwInterrupt(4) && statusIM(6)
  causeIP(5) := io.hwInterrupt(3) && statusIM(5)
  causeIP(4) := io.hwInterrupt(2) && statusIM(4)
  causeIP(3) := io.hwInterrupt(1) && statusIM(3)
  causeIP(2) := io.hwInterrupt(0) && statusIM(2)

  io.hwIntTrig := False
  val interrupt = False
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
    interrupt := swInterrupt || (hwInterrupt && io.hwIntMemAvail)
    when (interrupt) {
      exl := True
      causeExcCode := 0x00
      io.redirectEn := True
      io.redirectPc := pcArbiter.io.redirectPc
    }
    when (swInterrupt) {
      epc := io.write.pc + 4
    }
    when (hwInterrupt && io.hwIntMemAvail) {
      epc := io.write.pc
    }
  }

  pcArbiter.io.interrupt := interrupt

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