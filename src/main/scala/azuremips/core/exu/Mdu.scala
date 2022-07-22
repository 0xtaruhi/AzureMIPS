package azuremips.core.exu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Multiplier extends Component {
  val io = new Bundle {
    val valid = in Bool()
    val a     = in UInt(32 bits)
    val b     = in UInt(32 bits)
    val sign  = in Bool()
    val done  = out Bool() setAsReg
    val res   = out UInt(64 bits)
  }

  val a_sign = io.a.msb
  val b_sign = io.b.msb
  val a_u    = UInt(32 bits)
  val b_u    = UInt(32 bits)
  val res_u  = UInt(64 bits)

  when (io.sign) {
    a_u := Mux(a_sign, ~io.a + 1, io.a)
    b_u := Mux(b_sign, ~io.b + 1, io.b)
  } otherwise {
    a_u := io.a
    b_u := io.b
  }

  val product1 = RegNext(a_u(15 downto 0) * b_u(15 downto 0))
  val product2 = RegNext(a_u(15 downto 0) * b_u(31 downto 16))
  val product3 = RegNext(a_u(31 downto 16) * b_u(15 downto 0))
  val product4 = RegNext(a_u(31 downto 16) * b_u(31 downto 16))

  res_u := (product1 << 32) + ((product2 + product3) << 16) + (product4 << 32)
  io.res := Mux(io.sign & (a_sign ^ b_sign), ~res_u + 1, res_u)

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