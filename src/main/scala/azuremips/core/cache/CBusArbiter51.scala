package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class CBusArbiter51(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val icreq = in(new CReq())
    val icresp = out(new CResp())
    val dcreqs = Vec(in(new CReq()), config.dcache.portNum)
    val dcresps = Vec(out(new CResp()), config.dcache.portNum)
    val uncache_reqs = Vec(in(new DReq()), 2)
    val uncache_resps = Vec(out(new DResp()), 2)

    val creq = out(new CReq())
    val cresp = in(new CResp())
  }

  // val uncache_creqs = Vec(new CReq(), 2)
  val uncache_creqs = Vec(Reg(CReq()), 2)
  // for (i <- 0 until 2) {
  //   uncache_creqs(i).valid := io.uncache_reqs(i).paddr_valid
  //   uncache_creqs(i).is_write := io.uncache_reqs(i).strobe =/= U(0)
  //   uncache_creqs(i).size := io.uncache_reqs(i).size
  //   uncache_creqs(i).addr := io.uncache_reqs(i).paddr
  //   uncache_creqs(i).strobe := io.uncache_reqs(i).strobe
  //   uncache_creqs(i).data := io.uncache_reqs(i).data
  //   uncache_creqs(i).burst := CReq.AXI_BURST_FIXED
  //   uncache_creqs(i).len := CReq.MLEN1
  // }

  val uncache_resp_data = Vec(RegInit(U(0, 32 bits)), 2)
  val uncache_cresps = Vec(new CResp(), 2)
  
  val req_ports = Vec(CReq(), 5)
  val resp_ports = Vec(CResp(), 5)
  for (i <- 0 until 5) { // init
    resp_ports(i).ready := False
    resp_ports(i).last := False
    resp_ports(i).data := U(0)
  }

  req_ports(0) := uncache_creqs(0)
  req_ports(1) := uncache_creqs(1)
  req_ports(2) := io.dcreqs(0)
  req_ports(3) := io.dcreqs(1)
  req_ports(4) := io.icreq
  uncache_cresps(0) := resp_ports(0) 
  uncache_cresps(1) := resp_ports(1) 
  io.dcresps(0) := resp_ports(2)
  io.dcresps(1) := resp_ports(3)
  io.icresp := resp_ports(4) 

  val busy = RegInit(False)
  
  val selected = U(0, 3 bits)
  val selected_creq = req_ports(selected)
  val saved_creq = Reg(CReq())
  val index = RegInit(selected)
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
  for (i <- 4 to 0 by -1) {
    when (req_ports(i).valid) { selected := U(i).resized } 
  }

  // noIoPrefix()
  // uncache handshake fsm
  val fsm_uncache_handshake = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        uncache_creqs(0).valid := False
        uncache_creqs(1).valid := False
        when (io.uncache_reqs(0).paddr_valid) {
          uncache_creqs(0).valid := True
          uncache_creqs(0).is_write := io.uncache_reqs(0).strobe =/= U(0)
          uncache_creqs(0).size := io.uncache_reqs(0).size
          uncache_creqs(0).addr := io.uncache_reqs(0).paddr
          uncache_creqs(0).strobe := io.uncache_reqs(0).strobe
          uncache_creqs(0).data := io.uncache_reqs(0).data
          uncache_creqs(0).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(0).len := CReq.MLEN1
          goto(BUSY0)
        }.elsewhen(io.uncache_reqs(1).paddr_valid) {
          uncache_creqs(1).valid := True
          uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
          uncache_creqs(1).size := io.uncache_reqs(1).size
          uncache_creqs(1).addr := io.uncache_reqs(1).paddr
          uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
          uncache_creqs(1).data := io.uncache_reqs(1).data
          uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(1).len := CReq.MLEN1
          goto(BUSY1)
        }
      }
    }
    val BUSY0: State = new State {
      whenIsActive {
        when (uncache_cresps(0).last && io.uncache_reqs(1).paddr_valid) {
          uncache_creqs(1).valid := True // reg, so uncache_creqs(1).valid will be true next posedge but not now
          uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
          uncache_creqs(1).size := io.uncache_reqs(1).size
          uncache_creqs(1).addr := io.uncache_reqs(1).paddr
          uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
          uncache_creqs(1).data := io.uncache_reqs(1).data
          uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(1).len := CReq.MLEN1
          goto(BUSY1)
        }.elsewhen(uncache_cresps(0).last) {
          uncache_creqs(0).valid := False
          uncache_creqs(1).valid := False
          uncache_resp_data(0) := uncache_cresps(0).data // send to a reg

          goto(IDLE)
        }
      }
    } // BUSY0
    val BUSY1: State = new State {
      whenIsActive {
        when (uncache_cresps(1).last) {
          uncache_creqs(1).valid := False
          uncache_resp_data(1) := uncache_cresps(1).data // send to a reg
          uncache_creqs(0).valid := False
          when (io.uncache_reqs(0).paddr_valid) {
            uncache_creqs(0).valid := True
            uncache_creqs(0).is_write := io.uncache_reqs(0).strobe =/= U(0)
            uncache_creqs(0).size := io.uncache_reqs(0).size
            uncache_creqs(0).addr := io.uncache_reqs(0).paddr
            uncache_creqs(0).strobe := io.uncache_reqs(0).strobe
            uncache_creqs(0).data := io.uncache_reqs(0).data
            uncache_creqs(0).burst := CReq.AXI_BURST_FIXED
            uncache_creqs(0).len := CReq.MLEN1
            goto(BUSY0)
          }.elsewhen(io.uncache_reqs(1).paddr_valid) {
            uncache_creqs(1).valid := True
            uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
            uncache_creqs(1).size := io.uncache_reqs(1).size
            uncache_creqs(1).addr := io.uncache_reqs(1).paddr
            uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
            uncache_creqs(1).data := io.uncache_reqs(1).data
            uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
            uncache_creqs(1).len := CReq.MLEN1
            goto(BUSY1)
          }.otherwise {
            goto(IDLE)
          }
        }
      }
    } // BUSY1
  }
  io.uncache_resps(0).hit := uncache_cresps(0).last || fsm_uncache_handshake.isActive(fsm_uncache_handshake.BUSY1)
  io.uncache_resps(1).hit := uncache_cresps(1).last
  io.uncache_resps(0).data := uncache_resp_data(0)
  io.uncache_resps(1).data := uncache_resp_data(1)
}

object CBusArbiter51 {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).generateVerilog(new CBusArbiter51)
  }
}