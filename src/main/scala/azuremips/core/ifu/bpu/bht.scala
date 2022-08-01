package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._

class TwoBitCounter {
  val state = RegInit(WTAKE)

  def update(isTaken: Bool) = {
    val state_nxt = UInt(2 bits)
    switch(state) {
      is(SNTAKE) { state_nxt := isTaken ? WNTAKE | SNTAKE }
      is(WNTAKE) { state_nxt := isTaken ? STAKE | SNTAKE  }
      is(WTAKE)  { state_nxt := isTaken ? STAKE | SNTAKE  }
      is(STAKE)  { state_nxt := isTaken ? STAKE | WTAKE   }
    }
    state := state_nxt
  }

  def mayJmp: Bool = {
    state(1)
  }
}

object TwoBitCounter {
  def SNTAKE = U"00"
  def WNTAKE = U"01"
  def WTAKE  = U"10"
  def STAKE  = U"11"
}

case class Bht(config: IFConfig = IFConfig()) extends Component {
  val io = new Bundle {
    val vaddr = in UInt(32 bits)
    val redirectEn = in Bool()
    val redirectPc = in UInt(32 bits)
    val mayJmp = out Bool()
  }

  val reqAddr = getBhtAddr(io.vaddr)
  val updateAddr = getBhtAddr(io.redirectPc)

  val bhtVec = Vec(TwoBitCounter, config.bhtDepth) 
  val mayJmps = RegInit(U(0, config.bhtDepth bits))
  for (i <- 0 until config.bhtDepth) {
    mayJmps(i) := bhtVec(i).mayJmp
  }
  io.mayJmp = mayJm

  def getBhtAddr(vaddr: UInt): UInt = {
    vaddr(config.bhtUpperBound downto config.bhtLowerBound)
  }
}

object GenBhtVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new Bht())
  }
}