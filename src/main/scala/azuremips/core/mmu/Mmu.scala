package azuremips.core.mmu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._

case class Mmu() extends Component {
  val io = new Bundle {
    val vaddr     = in UInt(32 bits)
    val is_write  = in Bool()
    val uncache   = out Bool()
    val paddr     = out UInt(32 bits)
    val tlbPort   = master(TranslateAddrReq())
    val exptValid = out Bool()
    val exptCode  = out UInt(exptCodeWidth bits)
  }

  val kuseg  = !io.vaddr(31)
  val kseg0  = io.vaddr(31 downto 29) === U"100"
  val kseg1  = io.vaddr(31 downto 29) === U"101"
  val kseg23 = io.vaddr(31 downto 30) === U"11"

  val unmapped = io.vaddr(31 downto 30) === U"10"
  val mapped = io.vaddr(31 downto 30) =/= U"10"
  val cache  = Bool()
  
  io.tlbPort.vpn := io.vaddr(31 downto 12)
  when (unmapped) {
    io.paddr := U"000" @@ io.vaddr(28 downto 0)
    io.uncache := io.vaddr(29)
  } otherwise {
    io.paddr := io.tlbPort.pfn @@ io.vaddr(11 downto 0)
    io.uncache := !io.tlbPort.cache
  }
  io.exptCode := 0x0
  io.exptValid := False
  when(mapped && !io.tlbPort.found) {
    io.exptValid := True
    when (io.is_write) { io.exptCode := EXC_TLBREFILL_S }
    .otherwise { io.exptCode := EXC_TLBREFILL_L }
  }
  when (mapped && io.tlbPort.found && !io.tlbPort.valid) {
    io.exptValid := True
    when (io.is_write) { io.exptCode := EXC_TLBINVALID_S }
    .otherwise { io.exptCode := EXC_TLBINVALID_L }
  }
  when (mapped && io.tlbPort.found && io.tlbPort.valid && !io.tlbPort.dirty && io.is_write) {
    io.exptValid := True
    io.exptCode := EXC_TLBMOD
  }
}

object GenMMUVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Mmu)
  }
}