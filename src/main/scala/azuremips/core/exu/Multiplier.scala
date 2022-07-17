package azuremips.core.exu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Multiplier extends Component {
  val io = new Bundle {
    val op1      = in UInt(32 bits)
    val op2      = in UInt(32 bits)
    val start    = in Bool()
    val result   = out UInt(64 bits)
    val ready    = out Bool()         // able to accept new operation
    val valid    = out Bool()         // able to get result
  }

  io.valid  := True
  io.ready  := True
  io.result := io.op1 * io.op2
}

object GenMultiplierVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Multiplier)
  }
}