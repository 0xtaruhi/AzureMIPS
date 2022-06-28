package azuremips.core

import spinal.core._
import spinal.lib._

case class Execute(config: CoreConfig) extends Component {
  val io = new Bundle {
    val op1        = in UInt(32 bits)
    val op2        = in UInt(32 bits)
    val uop        = in UInt(Uops.uopWidth bits)

    val wenRegfile = out Bool()
    val wenMem     = out Bool()
    val wdata      = out UInt(32 bits)
    val wmask      = out UInt(4 bits)
  }

  io.wdata := 0
  io.wenRegfile := False
  io.wenMem := False
  io.wmask := 0
  switch (io.uop) {
    is (Uops.ALU_ADD) {
      io.wdata := io.op1 + io.op2
    }
    is (Uops.ALU_SUB) {
      io.wdata := io.op1 - io.op2
    }
    is (Uops.ALU_AND) {
      io.wdata := io.op1 & io.op2
    }
    is (Uops.ALU_OR) {
      io.wdata := io.op1 | io.op2
    }
    // is (Uops.ALU_XOR) {
    //   io.wdata := io.op1 ^ io.op2
    // }
  }
}

object Execute {
  def main(args: Array[String]) {
    SpinalVerilog(new Execute(CoreConfig()))
  }
}