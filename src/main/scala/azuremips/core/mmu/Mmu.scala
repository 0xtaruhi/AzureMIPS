package azuremips.core.mmu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class Mmu() extends Component {
  val io = new Bundle {
    val vaddr     = in UInt(32 bits)
    val uncache   = out Bool()
    val paddr     = out UInt(32 bits)
    val tlbPort   = master(TranslateAddrReq())
    val exptValid = out Bool()
    val exptCode  = out Bool()
  }

  val kuseg  = !io.vaddr(31)
  val kseg0  = io.vaddr(31 downto 29) === U"100"
  val kseg1  = io.vaddr(31 downto 29) === U"101"
  val kseg23 = io.vaddr(31 downto 30) === U"11"

  val mapped = kuseg || kseg23
  val cache  = Bool()
  // when (kseg0) {
  //   cache := 
  // }
  
  when (!mapped) {
    io.paddr := U"000" @@ io.vaddr(28 downto 0)
  } otherwise {

  }

}