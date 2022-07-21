package azuremips.core.exu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Multiplier extends Component {
  val io = new Bundle {
    val valid = in Bool()
    val a     = in UInt(32 bits)
    val b     = in UInt(32 bits)
    val done  = out Bool() setAsReg
    val res   = out UInt(64 bits)
  }

  val product1 = RegNext(io.a(15 downto 0) * io.b(15 downto 0))
  val product2 = RegNext(io.a(15 downto 0) * io.b(31 downto 16))
  val product3 = RegNext(io.a(31 downto 16) * io.b(15 downto 0))
  val product4 = RegNext(io.a(31 downto 16) * io.b(31 downto 16))

  io.res := (product1 << 32) + ((product2 + product3) << 16) + (product4 << 32)

    val fsm = new StateMachine {
    val sInit : State = new State with EntryPoint {
      whenIsActive {
        when (io.valid) {
          goto(sBusy)
          io.done := False
        }
      }
    }
    val sBusy : State = new State {
      whenIsActive {
        goto(sInit)
        io.done := True
      }
    }
  }
}

object Multiplier extends App {
  SpinalVerilog(new Multiplier)
}