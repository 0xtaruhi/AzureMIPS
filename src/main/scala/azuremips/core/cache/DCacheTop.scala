package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class DCacheTop(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val dreqs = Vec(in(new DReq()), config.dcache.portNum)
    val dresps = Vec(out(new DResp()), config.dcache.portNum)

    val creq = out(new CReq())
    val cresp = in(new CResp())
  }

  val dcache = new DCache()
  io.dreqs <> dcache.io.dreqs
  io.dresps <> dcache.io.dresps

  val carbtr = new CBusArbiter51()
  io.creq <> carbtr.io.creq
  io.cresp <> carbtr.io.cresp
  carbtr.io.dcreqs <> dcache.io.creqs
  carbtr.io.dcresps <> dcache.io.cresps
  val no_use_0 = carbtr.io.uncache_resps(0) 
  val no_use_1 = carbtr.io.uncache_resps(1) 
  val no_use_2 = carbtr.io.icresp
  val no_use_3 = new DReq()
  no_use_3.vaddr_valid := False
  no_use_3.paddr_valid := False
  no_use_3.vaddr := U(0) 
  no_use_3.paddr := U(0) 
  no_use_3.strobe := U(0)
  no_use_3.size := U(0)
  no_use_3.data := U(0)

  carbtr.io.uncache_reqs := Vec(no_use_3, 2)

  val mshr_creq = new CReq()
  mshr_creq.valid := False
  mshr_creq.is_write := False
  mshr_creq.size := CReq.MSIZE4
  mshr_creq.addr := U(0)
  mshr_creq.strobe := U"0000"
  mshr_creq.data := U(0)
  mshr_creq.burst := CReq.AXI_BURST_INCR
  mshr_creq.len := CReq.MLEN16

  carbtr.io.icreq := mshr_creq

  // noIoPrefix()
}

object DCacheTop {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new DCacheTop)
  }
}