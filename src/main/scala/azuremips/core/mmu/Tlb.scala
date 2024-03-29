package azuremips.core.mmu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class ProbeTlbReq() extends Bundle with IMasterSlave with TlbConfig {
  require(isPow2(tlbSize))
  val vpn2    = UInt(19 bits)
  // val asid    = UInt(8 bits)
  val found   = Bool()
  val index   = UInt(log2Up(tlbSize) bits)

  override def asMaster = {
    in(found, index)
    out(vpn2)
  }
}

case class ReadTlbPort() extends Bundle with IMasterSlave with TlbConfig {
  val index    = UInt(log2Up(tlbSize) bits)
  val entryHi  = UInt(32 bits)
  val pageMask = UInt(32 bits)
  val entryLo0 = UInt(32 bits)
  val entryLo1 = UInt(32 bits)

  override def asMaster = {
    out(index)
    in(entryHi, pageMask, entryLo0, entryLo1)
  }
}

case class WriteTlbPort() extends Bundle with IMasterSlave with TlbConfig {
  val wen      = Bool()
  val index    = UInt(log2Up(tlbSize) bits)
  val entryHi  = UInt(32 bits)
  val pageMask = UInt(32 bits)
  val entryLo0 = UInt(32 bits)
  val entryLo1 = UInt(32 bits)

  override def asMaster {
    out(wen, index, entryHi, pageMask, entryLo0, entryLo1)
  }
}

case class TranslateAddrReq() extends Bundle with IMasterSlave {
  val vpn       = UInt(20 bits)
  val pfn       = UInt(20 bits)
  val found     = Bool()
  val valid     = Bool()
  val cache     = Bool()
  val dirty     = Bool()

  override def asMaster {
    out(vpn)
    in(pfn, found, valid, cache, dirty)
  }
}

case class Tlb() extends Component with TlbConfig {
  val io = new Bundle {
    val asid  = in UInt(8 bits)
    val probe = slave(ProbeTlbReq())
    val read  = slave(ReadTlbPort())
    val write = slave(WriteTlbPort())
    val trans = Vec(slave(TranslateAddrReq()), 3)
  }

  val tlb = Vec(TlbEntry(), tlbSize)
  val tlbIndexWidth = log2Up(tlbSize)

  def getHitIndex(vpn2 : UInt) : UInt = {
    val hit_bits = UInt(tlbSize bits)
    for (i <- 0 until tlbSize) {
      hit_bits(i) := tlb(i).hit(io.asid, vpn2)
    }
    OHToUInt(hit_bits)
  }

  def tlbHit(vpn2 : UInt) : Bool = {
    tlb.map(_.hit(io.asid, vpn2)).reduce(_ || _)
  }

  // Probe
  io.probe.found := tlbHit(io.probe.vpn2)
  io.probe.index := getHitIndex(io.probe.vpn2)

  // Read
  io.read.entryHi  := tlb(io.read.index).getCp0EntryHi
  io.read.pageMask := tlb(io.read.index).getCp0PageMask
  io.read.entryLo0 := tlb(io.read.index).getCp0EntryLo0
  io.read.entryLo1 := tlb(io.read.index).getCp0EntryLo1

  // Write
  when (io.write.wen) {
    val writeTlbEntry = tlb(io.write.index)
    writeTlbEntry.entryHi.updateByCp0(io.write.entryHi)
    writeTlbEntry.pageMask := io.write.pageMask(24 downto 13)
    writeTlbEntry.global   := io.write.entryLo0(0) && io.write.entryLo1(0)
    writeTlbEntry.entryLo0.updateByCp0(io.write.entryLo0)
    writeTlbEntry.entryLo1.updateByCp0(io.write.entryLo1)
  }

  // Address Translate
  for (transPort <- io.trans) {
    val odd  = transPort.vpn(0)
    val vpn2 = transPort.vpn(19 downto 1)
    val index = getHitIndex(vpn2)
    // val entryLo = Mux(odd, tlb(index).entryLo1, tlb(index).entryLo0)
    transPort.found := tlbHit(vpn2)
    when(odd) {
      transPort.pfn   := tlb(index).entryLo1.pfn
      transPort.valid := tlb(index).entryLo1.valid
      transPort.cache := tlb(index).entryLo1.isCached
      transPort.dirty := tlb(index).entryLo1.dirty
    }.otherwise {
      transPort.pfn   := tlb(index).entryLo0.pfn
      transPort.valid := tlb(index).entryLo0.valid
      transPort.cache := tlb(index).entryLo0.isCached
      transPort.dirty := tlb(index).entryLo0.dirty
    }
  }

}

object GenTlbVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Tlb)
  }
}