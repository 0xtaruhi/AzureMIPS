package azuremips.core.mmu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class ProbeTlbReq() extends Bundle with IMasterSlave with TlbConfig {
  require(isPow2(tlbSize))
  val vpn2    = UInt(19 bits)
  val asid    = UInt(8 bits)
  val found   = Bool()
  val index   = UInt(log2Up(tlbSize) bits)

  override def asMaster = {
    in(found, index)
    out(vpn2, asid)
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

case class Tlb() extends Component with TlbConfig {
  val io = new Bundle {
    val probe = slave(ProbeTlbReq())
    val read  = slave(ReadTlbPort())
    val write = slave(WriteTlbPort())
  }

  val tlb = Vec(Reg(TlbEntry()) init (TlbEntry.emptyTlbEntry), tlbSize)
  val tlbIndexWidth = log2Up(tlbSize)

  // Probe
  io.probe.found := tlb.map(_.hit(io.probe.asid, io.probe.vpn2)).reduce(_ || _)
  io.probe.index := tlb.zipWithIndex.map {
    case (entry, index) => {
      val hit = entry.hit(io.probe.asid, io.probe.vpn2)
      Mux(hit, U(index, tlbIndexWidth bits), U(0, tlbIndexWidth bits))
    }
  }.reduce(_ | _)

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

}

object GenTlbVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Tlb)
  }
}