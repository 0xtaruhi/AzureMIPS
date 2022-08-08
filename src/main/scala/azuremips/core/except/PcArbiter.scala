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
    val redirectPc = out UInt(32 bits)
  }

  io.redirectPc := Mux(io.bev, U"32'h80000180", U"32'hbfc00380")
  when (io.interrupt) {
    switch (io.bev ## io.exl ## io.iv) {
      is (B"000") {
        io.redirectPc := U"32'h80000180"
      }
      is (B"001") {
        io.redirectPc := U"32'h80000200"
      }
      is (B"100") {
        io.redirectPc := U"32'hbfc00380"
      }
      is (B"101") {
        io.redirectPc := U"32'hbfc00400"
      }
    }
  } otherwise {
    switch (io.exptCode) {
      is (EXC_TLBREFILL_L, EXC_TLBREFILL_S) {
        switch (io.bev ## io.exl) {
          is (B"00") {
            io.redirectPc := U"32'h80000000"
          }
          is (B"01") {
            io.redirectPc := U"32'h80000180"
          }
          is (B"10") {
            io.redirectPc := U"32'hbfc00200"
          }
          is (B"11") {
            io.redirectPc := U"32'hbfc00380"
          }
        }
      }
    }
  }
}