package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class Btb() extends Component with BtbConfig {
  val io = new Bundle {
    val vaddr1 = in UInt(32 bits)
    val vaddr2 = in UInt(32 bits)
    val updatePc = in UInt(32 bits) // for addr
    val updateEn = in Bool()
    val actualTarget = in UInt(32 bits)
    val btbHit1 = out Bool()
    val btbHit2 = out Bool()
    val predictTarget1 = out UInt(32 bits)
    val predictTarget2 = out UInt(32 bits)
  }

  val btbRam = Mem(UInt(32 bits), btbSize)
  val tagRam = Mem(UInt(tagWidth bits), btbSize)
  val validRam = RegInit(U(0, btbSize bits))

  val btb_addr1 = getBtbAddr(io.vaddr1)
  val btb_addr2 = getBtbAddr(io.vaddr2)
  val vtag1 = getTag(io.vaddr1)
  val vtag2 = getTag(io.vaddr2)
  val refill_addr = getBtbAddr(io.updatePc)
  val refill_tag = getTag(io.updatePc)
  val refill_mask = Mux(io.updateEn, B"1", B"0")
  // tagRam & validRam
  val tag1 = tagRam(btb_addr1)
  val tag2 = tagRam(btb_addr2)
  tagRam.write(address=refill_addr, data=refill_tag, enable=io.updateEn, mask=refill_mask)
  val valid1 = validRam(btb_addr1)
  val valid2 = validRam(btb_addr2)
  when (io.updateEn) { validRam(refill_addr) := True }

  // dataRam
  btbRam.write(address=refill_addr, data=io.actualTarget, enable=io.updateEn, mask=refill_mask)
  io.predictTarget1 := btbRam.readSync(address=btb_addr1, enable=True)
  io.predictTarget2 := btbRam.readSync(address=btb_addr2, enable=True)

  val is_hit1 = RegNext(tag1 === vtag1 && valid1) init(False)
  val is_hit2 = RegNext(tag2 === vtag2 && valid2) init(False)
  io.btbHit1 := is_hit1
  io.btbHit2 := is_hit2

  def getTag(vaddr: UInt): UInt = {
    vaddr(tagUpperBound downto tagLowerBound)
  }

  def getBtbAddr(vaddr: UInt): UInt = {
    vaddr(indexUpperBound downto indexLowerBound)
  }
}

object GenBtbVerilog {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new Btb)
  }
}