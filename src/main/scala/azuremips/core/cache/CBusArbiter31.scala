package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class CBusArbiter31(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val icreq = in(new CReq())
    val icresp = out(new CResp())
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

  val req_ports = Vec(CReq(), 3)
  val resp_ports = Vec(CResp(), 3)
  for (i <- 0 until 3) {
    resp_ports(i).ready := False
    resp_ports(i).last := False
    resp_ports(i).data := U(0)
  }

  req_ports(0) := uncache_creq
  req_ports(1) := io.dcreq
  req_ports(2) := io.icreq
  uncache_cresp := resp_ports(0) 
  io.dcresp := resp_ports(1)
  io.icresp := resp_ports(2) 

  val busy = RegInit(False)
  
  val selected = UInt(2 bits)
  val selected_creq = req_ports(selected)
  val saved_creq = Reg(CReq())
  val index = RegInit(U(0, 2 bits))
  // busy
  when (busy && io.cresp.last) { 
    busy := False
    saved_creq.valid := False
  }.elsewhen(!busy) {
    busy := selected_creq.valid
    
  }
  // index, saved req
  when (!busy) {
    index := selected
    saved_creq := selected_creq
  }

  io.creq.valid := False
  io.creq.is_write := False
  io.creq.size := CReq.MSIZE4
  io.creq.addr := U(0)
  io.creq.strobe := U(0)
  io.creq.data := U(0)
  io.creq.burst := CReq.AXI_BURST_INCR
  io.creq.len := CReq.MLEN16
  when (busy) {
    io.creq := req_ports(index)
    resp_ports(index) := io.cresp
  }
  selected := U(0)
  for (i <- 2 to 0 by -1) {
    when (req_ports(i).valid) { selected := U(i).resized } 
  }

  noIoPrefix()
}

object CBusArbiter31 {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).generateVerilog(new CBusArbiter31)
  }
}