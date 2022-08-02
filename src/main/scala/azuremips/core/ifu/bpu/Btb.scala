package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._
import azuremips.core._

case class Btb() extends Component with BtbConfig {
  val io = new Bundle {
    val vaddr = in UInt(32 bits)
    val updatePc = in UInt(32 bits) // for addr
    val updateEn = in Bool()
    val actualTarget = in UInt(32 bits)
    val btbHit = out Bool()
    val predictTarget = out UInt(32 bits)
  }

  val btbRam = Mem(UInt(32 bits), btbSize)
  val tagRam = Mem(UInt(tagWidth bits), btbSize)
  val validRam = RegInit(U(0, btbSize bits))

  val btb_addr = getBtbAddr(io.vaddr)
  val vtag = getTag(io.vaddr)
  val refill_addr = getBtbAddr(io.updatePc)
  val refill_tag = getTag(io.updatePc)
  val refill_mask = Mux(io.updateEn, B"1", B"0")
  // tagRam & validRam
  val tag = tagRam.readAsync(address=btb_addr)
  tagRam.write(address=refill_addr, data=refill_tag, enable=io.updateEn, mask=refill_mask)
  val valid = validRam(btb_addr)
  when (io.updateEn) { validRam(refill_addr) := True }

  // dataRam
  btbRam.write(address=refill_addr, data=io.actualTarget, enable=io.updateEn, mask=refill_mask)
  io.predictTarget := btbRam.readSync(address=btb_addr, enable=True)

  val is_hit = RegNext(tag === vtag && valid) init(False)
  io.btbHit := is_hit

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