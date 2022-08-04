package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.ifu.bpu.TwoBitCounterStatus._

case class SelectArray() extends Component with SelectArrayConfig {
  val io = new Bundle {
    val pc            = in UInt(32 bits)
    val inTakenArray  = out Bool()
    val updateEn      = in Bool()
    val updatePc      = in UInt(32 bits)
    val updateTaken   = in Bool()
    val updateInTaken = out Bool()
  }
  val selectArray = Mem(TwoBitCounter(INIT), arrMemSize)

  val selectArea = new Area {
    val index  = io.pc(pcIndexRange)
    val offset = io.pc(pcOffsetRange)
    val arrMemAddr = getArrMemAddr(index, offset)

    io.inTakenArray := selectArray(arrMemAddr).predictTaken
  }

  val updateArea = new Area {
    val index  = io.updatePc(pcIndexRange)
    val offset = io.updatePc(pcOffsetRange)
    val arrMemAddr = getArrMemAddr(index, offset)

    val selectArrayResult = selectArray(arrMemAddr)
    val selectArrayNxt = selectArrayResult.updateWhen(io.updateTaken, io.updateEn)

    val resetSig = Bool()
    resetSig := ClockDomain.current.readResetWire
    val rstCounter = Reg(UInt(arrMemAddrWidth bits))
    rstCounter := rstCounter + U(1)
    val addr_for_wr = Mux(resetSig, rstCounter, arrMemAddr)
    val data_for_wr = Mux(resetSig, TwoBitCounter(INIT), selectArrayNxt)
    // write
    selectArray(addr_for_wr) := data_for_wr

    io.updateInTaken := selectArrayResult.predictTaken
  }

  def getArrMemAddr(index: UInt, offset: UInt) = {
    index @@ offset
  }

  def INIT = WeaklyTaken
}

object SelectArray {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new SelectArray)
  }
}