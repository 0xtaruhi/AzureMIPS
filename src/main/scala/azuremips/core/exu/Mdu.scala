package azuremips.core.exu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Multiplier extends Component {
  val io = new Bundle {
    val valid     = in Bool()
    val a         = in UInt(32 bits)
    val b         = in UInt(32 bits)
    val flush     = in Bool()
    val isSigned  = in Bool()
    val done      = out Bool() // setAsReg
    val res       = out UInt(64 bits)
  }

  val a_sign = io.a.msb
  val b_sign = io.b.msb
  val a_u    = UInt(32 bits)
  val b_u    = UInt(32 bits)
  val res_u  = UInt(64 bits)
  
  io.done := False

  when (io.isSigned) {
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
  when (io.flush) {
    product1 := U(0)
    product2 := U(0)
    product3 := U(0)
    product4 := U(0)
  }
  
  val prod_tmp = Vec(U(0, 64 bits), 4)
  prod_tmp(0)(31 downto 0) := product1
  prod_tmp(1)(47 downto 16) := product2
  prod_tmp(2)(47 downto 16) := product3
  prod_tmp(3)(63 downto 32) := product4
  res_u := prod_tmp.map(x => x).reduce(_ + _)
  io.res := Mux(io.isSigned & (a_sign ^ b_sign), ((~res_u) + 1), res_u)

  val fsm = new StateMachine {
    val sInit : State = new State with EntryPoint {
      whenIsActive {
        when (io.valid && !io.flush) {
          goto(sBusy)
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

class Divider extends Component {
  val io = new Bundle {
    val valid     = in Bool()
    val a         = in UInt(32 bits)
    val b         = in UInt(32 bits)
    val flush     = in Bool()
    val isSigned  = in Bool()
    val done      = out Bool() // setAsReg
    val res       = out UInt(64 bits)
  }

  val a_sign = io.a.msb
  val b_sign = io.b.msb
  val a_u    = UInt(32 bits)
  val b_u    = UInt(32 bits)

  when (io.isSigned) {
    a_u := Mux(a_sign, ~io.a + 1, io.a)
    b_u := Mux(b_sign, ~io.b + 1, io.b)
  } otherwise {
    a_u := io.a
    b_u := io.b
  }

  val prod = Reg(UInt(64 bits)) init(0)
  when (io.flush) {
    prod := U(0)
  }

  val fsm = new StateMachine {
    val count = RegInit(U(0, 35 bits))
    val INIT : State = new State with EntryPoint {
      whenIsActive {
        prod(31 downto 0) := a_u
        prod(63 downto 32) := U(0)
        when (io.valid && !io.flush) {
          count := INIT_COUNT
          goto(DOING)
        }
      }
    } // init end
    val DOING : State = new State {
      whenIsActive {
        when (prod(62 downto 31) >= b_u) {
          prod := U((63 downto 32) -> (prod(62 downto 31) - b_u), (31 downto 1) -> prod(30 downto 0), 0 -> True)
        }.otherwise {
          prod := prod |<< 1
        }

        count := count |>> 1
        when (count === U(1) || io.flush) {
          goto(INIT)
        }
      }
    } // doing end
  } // fsm end

  io.done := fsm.isEntering(fsm.INIT) && io.valid // && !io.flush

  io.res(63 downto 32) := (io.isSigned && (a_sign ^ prod.msb)) ? ((~prod(63 downto 32)) + 1) | prod(63 downto 32)
  io.res(31 downto 0) := (io.isSigned && (a_sign ^ b_sign)) ? ((~prod(31 downto 0)) + 1) | prod(31 downto 0)

  def INIT_COUNT = U((34 downto 32) -> U"001", (31 downto 0) -> False)
}

case class MulticycleInfo() extends Bundle {
  val multiplyValid = Bool()
  val divValid = Bool()
  val isSigned = Bool()
} 

object Multiplier extends App {
  SpinalVerilog(new Multiplier)
}

object Divider extends App {
  SpinalVerilog(new Divider)
}