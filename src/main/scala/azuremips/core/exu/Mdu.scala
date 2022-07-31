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

  val aIsSigned   = io.a.msb
  val bIsSigned   = io.b.msb
  val aUnsigned   = UInt(32 bits)
  val bUnsigned   = UInt(32 bits)
  val resUnsigned = UInt(64 bits)
  
  io.done := False

  when (io.isSigned) {
    aUnsigned := Mux(aIsSigned, ~io.a + 1, io.a)
    bUnsigned := Mux(bIsSigned, ~io.b + 1, io.b)
  } otherwise {
    aUnsigned := io.a
    bUnsigned := io.b
  }

  val product1 = RegNext(aUnsigned(15 downto 0)  * bUnsigned(15 downto 0) )
  val product2 = RegNext(aUnsigned(15 downto 0)  * bUnsigned(31 downto 16))
  val product3 = RegNext(aUnsigned(31 downto 16) * bUnsigned(15 downto 0) )
  val product4 = RegNext(aUnsigned(31 downto 16) * bUnsigned(31 downto 16))
  when (io.flush) {
    product1 := U(0)
    product2 := U(0)
    product3 := U(0)
    product4 := U(0)
  }
  
  val prodTmp = Vec(U(0, 64 bits), 4)
  prodTmp(0)(31 downto 0)  := product1
  prodTmp(1)(47 downto 16) := product2
  prodTmp(2)(47 downto 16) := product3
  prodTmp(3)(63 downto 32) := product4
  resUnsigned := (prodTmp(0) + prodTmp(1)) + (prodTmp(2) + prodTmp(3))
  io.res := Mux(io.isSigned & (aIsSigned ^ bIsSigned), ((~resUnsigned) + 1), resUnsigned)

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
        io.done := True && !io.flush
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

  val aIsSigned = io.a.msb
  val bIsSigned = io.b.msb
  val aUnsigned = UInt(32 bits)
  val bUnsigned = UInt(32 bits)

  val zero  = aUnsigned < bUnsigned
  val div16 = bUnsigned === U(16)
  val div10 = bUnsigned === U(10)
  // val quickFinished = Bool()

  // quickFinished := False

  when (io.isSigned) {
    aUnsigned := Mux(aIsSigned, ~io.a + 1, io.a)
    bUnsigned := Mux(bIsSigned, ~io.b + 1, io.b)
  } otherwise {
    aUnsigned := io.a
    bUnsigned := io.b
  }

  val prodOverall = UInt(64 bits)
  val productQuick = UInt(64 bits)
  val product = Reg(UInt(64 bits)) init(0)
  val q1Div10 = Reg(UInt(32 bits)) init(0)
  val q2Div10 = Reg(UInt(32 bits)) init(0)
  val qTmp    = UInt(32 bits)
  val rTmp    = UInt(32 bits)
  val rDiv10  = Reg(UInt(32 bits)) init(0)
  val specRes = Reg(Bool()) init False
  val productInit = UInt(64 bits)
  val productTmp = UInt(64 bits)
  val aUnsignedReg = Reg(UInt(32 bits)) init(0)
  val bUnsignedReg = Reg(UInt(32 bits)) init(0)
  when (io.flush) {
    product := U(0)
  }

  val divTmp = Vec(U(0, 32 bits), 3)

  productQuick := U(0)
  qTmp := U(0)
  rTmp := U(0)
  productInit := U(0)
  productTmp := U(0)

  val fsm = new StateMachine {
    val counter = RegInit(U(0, 6 bits))
    val sInit : State = new State with EntryPoint {
      whenIsActive {
        aUnsignedReg := aUnsigned
        bUnsignedReg := bUnsigned
        productInit := U((63 downto 32) -> False, (31 downto 0) -> aUnsigned)

        when (io.valid && div10 && !io.flush) {
          divTmp(0) := (aUnsigned |>> 1) + (aUnsigned |>> 2)
          q1Div10 := divTmp(0) + (divTmp(0) |>> 4)
          goto(sDiv10Stage1)
        }
        when (io.valid && !div10 && !div16 && !zero && !io.flush) {
          when (aUnsigned(31 downto 28).orR) {        // no skipping
            product := productInit
            counter := 0
            goto(sDoing)
          } otherwise {
            product := productInit |<< 4
            goto(sSkip)
          }
        }
        when (io.valid && div16 && !io.flush) {
          goto(sDiv16)
        }
        when (io.valid && zero && !io.flush) {
          goto(sZero)
        }
      }
    } // init end
    val sDoing : State = new State {
      whenIsActive {
        when (product(62 downto 31) >= bUnsignedReg) {
          productTmp := U((63 downto 32) -> (product(62 downto 31) - bUnsignedReg), (31 downto 1) -> product(30 downto 0), 0 -> True)
        } otherwise {
          productTmp := product |<< 1
        }
        when (productTmp(62 downto 31) >= bUnsignedReg) {
          product := U((63 downto 32) -> (productTmp(62 downto 31) - bUnsignedReg), (31 downto 1) -> productTmp(30 downto 0), 0 -> True)
        } otherwise {
          product := productTmp |<< 1
        }

        counter := counter + 2
        when (counter === U(32) || io.flush) {
          goto(sInit)
        }
      }
    } // doing end
    val sDiv10Stage1 : State = new State {
      whenIsActive {
        divTmp(1) := q1Div10 + (q1Div10 |>> 8)
        divTmp(2) := divTmp(1) + (divTmp(1) |>> 16)
        qTmp := divTmp(2) |>> 3
        rTmp := aUnsignedReg - (((qTmp |<< 2) + qTmp) |<< 1);
        q2Div10 := qTmp
        rDiv10 := rTmp
        specRes := (rTmp > 9)
        when (io.flush) {
          goto(sInit)
        } otherwise {
          goto(sDiv10Stage2)
        }
      }
    }
    val sDiv10Stage2 : State = new State {
      whenIsActive {
        when (specRes) {
          productQuick := U((63 downto 32) -> (rDiv10 - 10), (31 downto 0) -> (q2Div10 + 1))
        } otherwise {
          productQuick := U((63 downto 32) -> rDiv10, (31 downto 0) -> q2Div10)
        }
        goto(sInit)
      }
    }
    val sSkip : State = new State {
      whenIsActive {
        when (product(31 downto 28).orR) {        // no skipping
            product := product
            counter := 4
          } elsewhen (product(27 downto 24).orR) {  // 4 bits zero
            product := product |<< 4
            counter := 8
          } elsewhen (product(23 downto 20).orR) {  // 8 bits zero
            product := product |<< 8
            counter := 12
          } elsewhen (product(19 downto 16).orR) {
            product := product |<< 12
            counter := 16
          } elsewhen (product(15 downto 12).orR) {  // 16 bits zero
            product := product |<< 16
            counter := 20
          } elsewhen (product(11 downto 8).orR) {
            product := product |<< 20
            counter := 24
          } otherwise {
            product := product |<< 24
            counter := 28
          }
        when (io.flush) {
          goto(sInit)
        } otherwise {
          goto(sDoing)
        }
      }
    }
    val sDiv16 : State = new State {
      whenIsActive {
        productQuick(31 downto 0) := aUnsigned |>> 4
        productQuick(63 downto 32) := aUnsigned(3 downto 0).resize(32)
        goto(sInit)
      }
    }
    val sZero : State = new State {
      whenIsActive {
        productQuick(31 downto 0) := U(0)
        productQuick(63 downto 32) := aUnsigned
        goto(sInit)
      }
    }
  } // fsm end

  io.done := (fsm.isEntering(fsm.sInit) && io.valid) && !io.flush

  when (io.flush) {
    prodOverall := U(0)
  } elsewhen (div10 || div16 || zero) {
    prodOverall := productQuick
  } otherwise {
    prodOverall := product
  }

  io.res(63 downto 32) := (io.isSigned && (aIsSigned ^ prodOverall.msb)) ? ((~prodOverall(63 downto 32)) + 1) | prodOverall(63 downto 32)
  io.res(31 downto 0) := (io.isSigned && (aIsSigned ^ bIsSigned)) ? ((~prodOverall(31 downto 0)) + 1) | prodOverall(31 downto 0)

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