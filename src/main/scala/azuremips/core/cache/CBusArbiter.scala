package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class CBusArbiter(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val icreq = in(new CReq())
    val icresp = in(new CResp())
    val dcreq = in(new CReq())
    val dcresp = out(new CResp())
    val uncache_req = in(new DReq())
    val uncache_resp = out(new DResp())

    val creq = out(new CReq())
    val cresp = in(new CResp())
  }

  val uncache_creq = new CReq()
  uncache_creq.valid := io.uncache_req.paddr_valid
  uncache_creq.is_write := io.uncache_req.strobe =/= U(0)
  uncache_creq.size := io.uncache_req.size
  uncache_creq.addr := io.uncache_req.paddr
  uncache_creq.strobe := io.uncache_req.strobe
  uncache_creq.data := io.uncache_req.data
  uncache_creq.burst := CReq.AXI_BURST_FIXED
  uncache_creq.len := CReq.MLEN1

  val uncache_resp_data = RegNext(io.cresp.data)
  val uncache_cresp = new CResp()
  io.uncache_resp.hit := uncache_cresp.last
  io.uncache_resp.data := uncache_resp_data

  // val req_ports = Vec()
}

object CBusArbiter {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new CBusArbiter)
  }
}