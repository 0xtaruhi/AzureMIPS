package azuremips.core.except

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ExceptionCode._

case class PcArbiter() extends Component {
  val io = new Bundle {
    val interrupt  = in Bool()
    val exptCode   = in UInt(exptCodeWidth bits)
    val bev        = in Bool()
    val exl        = in Bool()
    val iv         = in Bool()
    val eBase      = in UInt(32 bits)
    val redirectPc = out UInt(32 bits)
  }

  val vectorBaseAddress = UInt(32 bits)
  when (io.bev) {
    vectorBaseAddress := U"32'hbfc00200"
  } otherwise {
    vectorBaseAddress := io.eBase(31 downto 12) @@ U(0, 12 bits)
  }

  val vectorOffset = UInt(10 bits)
  when (io.exl) {
    vectorOffset := 0x180
  } otherwise {
    when (io.exptCode === EXC_TLBREFILL_L || io.exptCode === EXC_TLBREFILL_S) {
      vectorOffset := 0
    } elsewhen (io.interrupt) {
      when (io.iv === False) {
        vectorOffset := 0x180
      } otherwise {
        vectorOffset := 0x200
      }
    } otherwise {
      vectorOffset := 0x180
    }
  }
  io.redirectPc := vectorBaseAddress(31 downto 11) @@ (vectorBaseAddress(10 downto 0) + vectorOffset)
}