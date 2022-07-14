package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.utils.Log2

case class Divider(
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val req       = in Bool()
    val ready     = out Bool()
    val dividend  = in UInt(32 bits)
    val divisor   = in UInt(32 bits)
    val sign      = in Bool()
    val quotient  = out UInt(32 bits)
    val remainder = out UInt(32 bits)
    val valid     = out Bool()
  }
  
  object DividerStatus extends SpinalEnum {
    val sIdle, sLog2, sShift, sCompute, sFinish = newElement()
  }

  def abs(x: UInt, sign: Bool): (Bool, UInt) = {
    val s = a.msb && sign
    (s, Mux(s, U(0)-x, x))
  }

  val state  = RegInit(DividerStatus.sIdle)
  val newReq = (state === DividerStatus.sIdle) && io.req
  val divBy0 = io.divisor === 0
  val shiftReg = Reg(UInt(65 bits))
  val (a, b) = (io.dividend, io.divisor)
  val hi = shiftReg(64 downto 32)
  val lo = shiftReg(31 downto 0)

  val (aSign, aVal) = abs(a, io.sign)
  val (bSign, bVal) = abs(b, io.sign)
  val aSignReg = RegNextWhen(aSign, newReq)
  val qSignReg = RegNextWhen((aSign ^ bSign) && !divBy0, newReq)
  val bReg = RegNextWhen(bVal, newReq)
  val aValx2Reg = RegNextWhen(aVal << 1, newReq)

  val cnt = Reg(UInt(5 bits))

  when (newReq) {
    state := DividerStatus.sLog2
  } elsewhen (state === DividerStatus.sLog2) {
    val canSkipShift = (U(32) | Log2(bReg)) - Log2(aValx2Reg)
    cnt := Mux(divBy0, U(0), Mux(canSkipShift >= U(31), U(31), canSkipShift))
  } elsewhen (state === DividerStatus.sShift) {
    shiftReg := aValx2Reg << cnt
    state := DividerStatus.sCompute
  } elsewhen (state === DividerStatus.sCompute) {
    val enough = (hi >= bReg)
    shiftReg := Mux(enough, hi - bReg, hi)(31 downto 0) @@ lo @@ U(enough)
    cnt := cnt + 1
  } elsewhen (state === DividerStatus.sFinish) {
    state := DividerStatus.sIdle
  }

  val r = hi(32 downto 1)
  val resQ = Mux(qSignReg, U(0) - lo, lo)
  val resR = Mux(aSignReg, U(0) - r, r)
  io.quotient := resQ
  io.remainder := resR

  io.ready := (state === DividerStatus.sIdle)
  io.valid := (state === DividerStatus.sFinish)
}
