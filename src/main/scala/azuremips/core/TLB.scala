package azuremips.core

import spinal.core._
import spinal.lib._

import azuremips.core._

class TLB(pipeline: Boolean = false,
          config: CoreConfig = CoreConfig()) extends Component {

  val io = new Bundle {
    val vaddr = in UInt(config.vaddrWidth bits)
    val paddr = out UInt(config.paddrWidth bits)
  }

  val convertedAddr = UInt(config.paddrWidth bits)
  val addrMask      = UInt(config.paddrWidth bits)

  // kseg0 & kseg1(0x8000_0000 - 0xBFFF_FFFF) map to 0x0000_0000
  when (io.vaddr(io.vaddr.high downto io.vaddr.high-1) === U"2'b10") {
    addrMask := U"h0FFF_FFFF"
  } otherwise {
    addrMask := U"hFFFF_FFFF"
  }
  convertedAddr := addrMask & io.vaddr

  if (pipeline) {
    io.paddr := RegNext(convertedAddr)
  } else {
    io.paddr := convertedAddr
  }
}

object TLB {
  def apply(vaddr: UInt, paddr: UInt) {
    apply(vaddr, paddr, pipeline=false)
  }

  def apply(vaddr: UInt, paddr: UInt, pipeline: Boolean) {
    val tlb = new TLB(pipeline)
    tlb.io.vaddr := vaddr
    paddr := tlb.io.paddr
  }
}

object TLBVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new TLB(true))
  }
}