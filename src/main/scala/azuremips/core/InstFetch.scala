package azuremips.core

import spinal.core._
import spinal.lib._

case class InstFetch(config: CoreConfig) extends Component {
  val io = new Bundle {
    val redirectEn = in Bool()
    val redirectPc = in UInt(config.vaddrWidth bits)
    val pc         = out UInt(config.vaddrWidth bits)
  }

  val curPc = Reg(UInt(config.vaddrWidth bits)) init (config.initPc)
  when (io.redirectEn) {
    curPc := io.redirectPc
  } otherwise {
    curPc := curPc + 4
  }
  io.pc := curPc
}

object InstFetch {
  def main(args: Array[String]) {
    SpinalVerilog(InstFetch(CoreConfig()))
  }
}