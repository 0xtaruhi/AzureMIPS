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

  val uncache_creqs = Vec(RegInit(CReq.emptyCReq), 2)

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
  val selected_creq = CReq()
  val saved_creq = Reg(CReq())
  val index = RegInit(U(0, 3 bits))
  
  for (i <- 4 to 0 by -1) {
    when (req_ports(i).valid) { selected := U(i).resized } 
  }
  selected_creq := req_ports(selected)

  io.creq := CReq.emptyCReq
  when (busy) {
    io.creq := req_ports(index)
    resp_ports(index) := io.cresp
  }
  // direct translation
  // io.creq.addr(31 downto 29) := U(0, 3 bits)

  // busy
  when (busy && io.cresp.last) { 
    busy := False
    saved_creq := CReq.emptyCReq
  }.elsewhen(!busy) {
    busy := selected_creq.valid
    index := selected
    saved_creq := selected_creq
  }

  // noIoPrefix()
  // uncache handshake fsm
  val fsm_uncache_handshake = new StateMachine {
    val is_two_req = RegInit(False)
    val need_1_hit = RegInit(False)
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        uncache_creqs(0).valid := False
        uncache_creqs(1).valid := False
        need_1_hit := False
        // creq0 valid not included
        uncache_creqs(0).is_write := io.uncache_reqs(0).strobe =/= U(0)
        uncache_creqs(0).size := io.uncache_reqs(0).size
        uncache_creqs(0).addr := io.uncache_reqs(0).paddr
        uncache_creqs(0).strobe := io.uncache_reqs(0).strobe
        uncache_creqs(0).data := io.uncache_reqs(0).data
        uncache_creqs(0).burst := CReq.AXI_BURST_FIXED
        uncache_creqs(0).len := CReq.MLEN1
        // creq1 valid not included
        uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
        uncache_creqs(1).size := io.uncache_reqs(1).size
        uncache_creqs(1).addr := io.uncache_reqs(1).paddr
        uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
        uncache_creqs(1).data := io.uncache_reqs(1).data
        uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
        uncache_creqs(1).len := CReq.MLEN1
        when (io.uncache_reqs(0).paddr_valid) {
          uncache_creqs(0).valid := True
          goto(BUSY0)
        }.elsewhen(io.uncache_reqs(1).paddr_valid) {
          uncache_creqs(1).valid := True
          goto(BUSY1)
        }
        is_two_req := io.uncache_reqs(0).paddr_valid && io.uncache_reqs(1).paddr_valid
      }
    }
    val BUSY0: State = new State {
      whenIsActive {
        when (uncache_cresps(0).last && is_two_req) {
          uncache_creqs(1).valid := True // reg, so uncache_creqs(1).valid will be true next posedge but not now
          uncache_creqs(0).valid := False
          need_1_hit := True
          goto(BUSY1)
        }.elsewhen(uncache_cresps(0).last) {
          need_1_hit := False

          is_two_req := io.uncache_reqs(0).paddr_valid && io.uncache_reqs(1).paddr_valid
          uncache_creqs(1).valid := False
          uncache_creqs(0).valid := False

          uncache_creqs(0).is_write := io.uncache_reqs(0).strobe =/= U(0)
          uncache_creqs(0).size := io.uncache_reqs(0).size
          uncache_creqs(0).addr := io.uncache_reqs(0).paddr
          uncache_creqs(0).strobe := io.uncache_reqs(0).strobe
          uncache_creqs(0).data := io.uncache_reqs(0).data
          uncache_creqs(0).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(0).len := CReq.MLEN1
          // creq1 valid not included
          uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
          uncache_creqs(1).size := io.uncache_reqs(1).size
          uncache_creqs(1).addr := io.uncache_reqs(1).paddr
          uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
          uncache_creqs(1).data := io.uncache_reqs(1).data
          uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(1).len := CReq.MLEN1
          
          when (io.uncache_reqs(0).paddr_valid) {
            uncache_creqs(0).valid := True
            goto(BUSY0)
          }.elsewhen(io.uncache_reqs(1).paddr_valid) {
            uncache_creqs(1).valid := True
            goto(BUSY1)
          }.otherwise {
            goto(IDLE)
          }          
        }
        // when (uncache_cresps(0).last) {
        //   uncache_resp_data(0) := uncache_cresps(0).data
        // }
      }
    } // BUSY0
    val BUSY1: State = new State {
      whenIsActive {
        when (uncache_cresps(1).last) {
          need_1_hit := False
          
          uncache_creqs(1).valid := False
          uncache_creqs(0).valid := False

          uncache_creqs(0).is_write := io.uncache_reqs(0).strobe =/= U(0)
          uncache_creqs(0).size := io.uncache_reqs(0).size
          uncache_creqs(0).addr := io.uncache_reqs(0).paddr
          uncache_creqs(0).strobe := io.uncache_reqs(0).strobe
          uncache_creqs(0).data := io.uncache_reqs(0).data
          uncache_creqs(0).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(0).len := CReq.MLEN1
          // creq1 valid not included
          uncache_creqs(1).is_write := io.uncache_reqs(1).strobe =/= U(0)
          uncache_creqs(1).size := io.uncache_reqs(1).size
          uncache_creqs(1).addr := io.uncache_reqs(1).paddr
          uncache_creqs(1).strobe := io.uncache_reqs(1).strobe
          uncache_creqs(1).data := io.uncache_reqs(1).data
          uncache_creqs(1).burst := CReq.AXI_BURST_FIXED
          uncache_creqs(1).len := CReq.MLEN1
          
          when (io.uncache_reqs(0).paddr_valid) {
            uncache_creqs(0).valid := True
            goto(BUSY0)
          }.elsewhen(io.uncache_reqs(1).paddr_valid) {
            uncache_creqs(1).valid := True
            goto(BUSY1)
          }.otherwise {
            goto(IDLE)
          }

          // uncache_resp_data(1) := uncache_cresps(1).data
        } // when cbus1 resp.last === True end
        is_two_req := io.uncache_reqs(0).paddr_valid && io.uncache_reqs(1).paddr_valid
      }
    } // BUSY1
  }
  io.uncache_resps(0).hit := uncache_cresps(0).last || fsm_uncache_handshake.need_1_hit
  io.uncache_resps(1).hit := uncache_cresps(1).last
  for(i <- 0 until 2) {
    when (uncache_cresps(i).last) { uncache_resp_data(i) := uncache_cresps(i).data }
  }
  io.uncache_resps(0).data := uncache_resp_data(0) // 1 clock after
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