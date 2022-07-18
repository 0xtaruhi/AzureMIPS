package azuremips.core

import spinal.core._
import spinal.lib._

import azuremips.core.cache._

case class TopCore(config: CoreConfig = CoreConfig()) extends Component {
  val io  = new Bundle {
    val oreq = out(new CReq())
    val oresp = in(new CResp())
    val ext_int = in(UInt(6 bits)) // exception signal
  }

  noIoPrefix() // can connect VTop.sv with (.*)

  val dcache = new DCache()
  val icache = new ICache()
  val arbiter51 = new CBusArbiter51()

  dcache.io.creqs <> arbiter51.io.dcreqs
  dcache.io.cresps <> arbiter51.io.dcresps
  icache.io.cresp <> arbiter51.io.icresp
  icache.io.creq <> arbiter51.io.icreq
  io.oresp <> arbiter51.io.cresp
  io.oreq <> arbiter51.io.creq
}

object TopCore {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new TopCore)
  }
}