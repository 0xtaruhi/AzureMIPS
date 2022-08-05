package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ifu.bpu.TwoBitCounterStatus._

case class PredictArray() extends Component with BhtConfig with GhrConfig {
  val io = new Bundle {
    val pc            = in UInt(32 bits)
    val ghr           = in UInt(ghrSize bits)
    val inTakenArray  = in Bool()    
    val predictTaken  = out Bool() setAsReg

    val updateEn      = in Bool()
    val updateInTaken = in Bool()
    val updatePc      = in UInt(32 bits)
    val updateTaken   = in Bool()
  }

  val takenArray = Mem(TwoBitCounter(WeaklyTaken), arrMemSize)
  val notTakenArray = Mem(TwoBitCounter(WeaklyNotTaken), arrMemSize)

  val selectArea = new Area {
    val index  = getIndex(io.pc, io.ghr)
    val offset = getOffset(io.pc, io.ghr)
    val arrMemAddr = getArrMemAddr(index, offset)
    
    val takenArrayResult = takenArray(arrMemAddr).predictTaken
    val notTakenArrayResult = notTakenArray(arrMemAddr).predictTaken

    io.predictTaken := Mux(io.inTakenArray, takenArrayResult, notTakenArrayResult)
  }

  val updateArea = new Area {
    val index  = getIndex(io.updatePc, io.ghr)
    val offset = getOffset(io.updatePc, io.ghr)
    val arrMemAddr = getArrMemAddr(index, offset)
    // no read en
    val takenArrayNxt = takenArray(arrMemAddr).updateWhen(io.updateTaken, io.updateInTaken)
    val notTakenArrayNxt = notTakenArray(arrMemAddr).updateWhen(io.updateTaken, !io.updateInTaken)

    val resetSig = Bool()
    resetSig := ClockDomain.current.readResetWire
    val rstCounter = Reg(UInt(arrMemAddrWidth bits))
    rstCounter := rstCounter + U(1)
    val addr_for_wr = Mux(resetSig, rstCounter, arrMemAddr)
    val data_for_wr_taken = Mux(resetSig, TwoBitCounter(WeaklyTaken), takenArrayNxt)
    val data_for_wr_notTaken = Mux(resetSig, TwoBitCounter(WeaklyNotTaken), notTakenArrayNxt)
    // write
    when (io.updateEn || resetSig) {
      takenArray(addr_for_wr) := data_for_wr_taken
      notTakenArray(addr_for_wr) := data_for_wr_notTaken
    }
  }

  def getOffset(pc : UInt, ghr : UInt) = {
    val offset = pc((2 + offsetWidth) downto 3) ^ ghr((offsetWidth - 1) downto 0)
    offset
  }

  def getIndex(pc : UInt, ghr : UInt) = {
    val index = ghr(9 downto 8) @@ (ghr(7 downto 4) ^ ghr(3 downto 0))
    // val index = (ghr(9 downto 6) ^ ghr(5 downto 2)) @@ ghr(1 downto 0)
    // val index = ghr(7 downto 4) @@ (ghr(3 downto 2) ^ ghr(1 downto 0)) // 8 + lo
    // val index = (ghr(7 downto 6) ^ ghr(5 downto 4)) @@ ghr(3 downto 0) // 8 + hi
    // val index = ghr(5 downto 4) @@ (ghr(3 downto 2) ^ ghr(1 downto 0))
    // val index = U(ghr(5) ^ ghr(4)) @@ ghr(3 downto 0)
    // val index = ghr
    index
  }

  def getArrMemAddr(index: UInt, offset: UInt) = {
    index @@ offset
  }
}

object PredictArray {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new PredictArray)
  }
}