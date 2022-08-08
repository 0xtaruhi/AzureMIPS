package azuremips.core.mmu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class EntryHiSignals() extends Bundle {
  val vpn2 = Reg(UInt(19 bits)) init (0)
  val asid = Reg(UInt(8 bits)) init (0)

  def updateByCp0(info : UInt) = {
    vpn2 := info(31 downto 13)
    asid := info(7 downto 0)    
  }
}

object EntryHiSignals {
  def emptyEntryHiSignals = {
    val s = EntryHiSignals()
    s.vpn2 := 0
    s.asid := 0
    s
  }

}

case class EntryLoSignals() extends Bundle {
  val pfn    = Reg(UInt(20 bits)) init (0)
  val cache  = Reg(UInt(3 bits)) init (0)
  val dirty  = Reg(Bool()) init (False)
  val valid  = Reg(Bool()) init (False)

  def getControlSignals() = {
    cache @@ dirty @@ valid
  }

  def updateByCp0(info : UInt) = {
    pfn := info(25 downto 6)
    cache := info(5 downto 3)
    dirty := info(2)
    valid := info(1)
  }

  def isCached = (cache === 3)
}

object EntryLoSignals {
  def emptyEntryLoSignals = {
    val s = EntryLoSignals()
    s.pfn    := 0
    s.cache  := 0
    s.dirty  := False
    s.valid  := False
    s
  }
}

case class TlbEntry() extends Bundle {
  val entryHi  = EntryHiSignals()
  val pageMask = Reg(UInt(12 bits)) init (0)
  val global   = Reg(Bool()) init (False)
  val entryLo0 = EntryLoSignals()
  val entryLo1 = EntryLoSignals()

  def getCp0EntryHi() = {
    entryHi.vpn2 @@ U(0, 5 bits) @@ entryHi.asid
  }

  def getCp0EntryLo0() = {
    U(0, 6 bits) @@ entryLo0.pfn @@ entryLo0.getControlSignals() @@ global
  }

  def getCp0EntryLo1() = {
    U(0, 6 bits) @@ entryLo1.pfn @@ entryLo1.getControlSignals()  @@ global
  }

  def getCp0PageMask() = {
    U(0, 7 bits) @@ pageMask @@ U(0, 13 bits)
  }

  def hit(asid : UInt, vpn2 : UInt) = {
    (this.entryHi.asid === asid || this.global) && vpn2 === this.entryHi.vpn2
  }

}

object TlbEntry {
  def emptyTlbEntry = {
    val s = new TlbEntry
    s.entryHi  := EntryHiSignals.emptyEntryHiSignals
    s.pageMask := 0
    s.global   := False
    s.entryLo0 := EntryLoSignals.emptyEntryLoSignals
    s.entryLo1 := EntryLoSignals.emptyEntryLoSignals
    s
  }
}