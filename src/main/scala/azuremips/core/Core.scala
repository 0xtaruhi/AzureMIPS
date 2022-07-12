package azuremips.core

import spinal.core._
import spinal.lib._

import azuremips.core._

class Core(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val cresp = in(new cache.CResp())
    val creq  = out(new cache.CReq())
  }

  val icache    = cache.ICache(config)
  val instFetch = new ifu.InstFetch(config)
  instFetch.io.icache <> icache.io.fetch_if
  icache.io.creq <> io.creq
  icache.io.cresp <> io.cresp

  instFetch.io.ifJmp.jmpEn := False
  instFetch.io.ifJmp.jmpDest := 0
  instFetch.io.fetchBufFull := False
}

object genCoreVerilog {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new Core(CoreConfig()))
  }
}