package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core.ifu.IF2ICache
import azuremips.core._

case class ICache(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val fetch_if = slave(IF2ICache(config))
    // val stall_all = in Bool() // from controlFlow
    val cresp = in(new CResp())
    val creq = out(new CReq())
    // cache insts 
    val cache_inst_info = in(new CacheInstInfo())
    val tag_for_index_store = in UInt(config.icache.tagWidth bits)
  }
  
  val stall_12 = False
  val stall_23 = False
  // some rename
  val icachecfg = config.icache
  val v_indexes = Vec(UInt(icachecfg.indexWidth bits), icachecfg.portNum)
  v_indexes(THIS) := getVIndex(io.fetch_if.vaddr) 
  v_indexes(NL) := getVIndex(io.fetch_if.vaddr) + U(1)
  val v_indexes12 = RegInit(Vec(U(0, icachecfg.indexWidth bits), icachecfg.portNum))
  when (!stall_12) {
    v_indexes12 := v_indexes
  }
  val vaddr_valid = io.fetch_if.vaddr_valid
  val paddrs = Vec(UInt(32 bits), icachecfg.portNum)
  paddrs(THIS) := io.fetch_if.paddr
  paddrs(NL) := getNLPAddr(io.fetch_if.paddr, v_indexes12(NL))
  val paddr_valid = io.fetch_if.paddr_valid
  
  val is_refill = Bool()
  val has_fsm_loadings = Vec(Bool(), icachecfg.portNum)
  // creq initialisation, otherwise latch
  io.creq.valid := False
  io.creq.is_write := False
  io.creq.size := CReq.MSIZE4
  io.creq.addr := U(0)
  io.creq.strobe := U(0)
  io.creq.data := U(0)
  io.creq.burst := CReq.AXI_BURST_INCR
  io.creq.len := CReq.MLEN16
  
  // tag ram
  val tagRam = for (i <- 0 until icachecfg.portNum) yield {
    Mem(UInt(icachecfg.tagRamWordWidth bits), icachecfg.setNum)
  }
  // meta, i.e. valid ram
  // val validRam_nxt = Vec(UInt(icachecfg.validRamWordWidth bits), icachecfg.setNum)
  val validRam = Vec(RegInit(U(0, icachecfg.validRamWordWidth bits)), icachecfg.setNum)
  // validRam_nxt := validRam
  // when(meta_refresh_stall) {
  //   validRam := validRam_nxt
  // }
  // data ram, banks yield
  val dataRam = for (i <- 0 until icachecfg.bankNum) yield {
    Mem(UInt(icachecfg.dataRamWordWidth bits), icachecfg.bankSize)
  }
  // stage 1
  
  val tags = Vec(UInt(icachecfg.tagRamWordWidth bits), icachecfg.portNum)
  for(i <- 0 until icachecfg.portNum) {
    tags(i) := tagRam(i).readAsync(address=v_indexes(i))
  }
  val tags_for_match = Vec(Vec(UInt(icachecfg.tagWidth bits), icachecfg.wayNum), icachecfg.portNum)
  for(i <- 0 until icachecfg.portNum) {
    for (j <- 0 until icachecfg.wayNum) {
      tags_for_match(i)(j) := tags(i)(j*icachecfg.tagWidth+icachecfg.tagWidth-1 downto j*icachecfg.tagWidth)
    }
  }

  val valids = Vec(UInt(icachecfg.validRamWordWidth bits), icachecfg.portNum)
  for(i <- 0 until icachecfg.portNum) {
    valids(i) := validRam(v_indexes(i))
  }
  val offset12_nxt = io.fetch_if.vaddr(icachecfg.offsetUpperBound downto icachecfg.offsetLowerBound)
  val is_crossline12_nxt = io.fetch_if.vaddr(icachecfg.offsetUpperBound downto icachecfg.offsetLowerBound) > U"1100" && vaddr_valid
  // hold output io.insts index to look up which bank is the inst come from
  val which_bank = Vec(UInt(icachecfg.bankIdxWidth bits), icachecfg.bankNum)
  val which_output_port = Vec(UInt(icachecfg.bankIdxWidth bits), icachecfg.bankNum)
  val which_line = Vec(UInt(icachecfg.portIdxWidth bits), icachecfg.bankNum)
  val inst0_bankId = getBankId(offset12_nxt)
  for (i <- 0 until icachecfg.bankNum) {
    which_bank(i) := U(i, icachecfg.bankIdxWidth bits) + inst0_bankId
    which_output_port(i) := U(i + 1).resized + (~inst0_bankId) // actually this is i - inst0_bankId
  }
  for (i <- 0 until icachecfg.bankNum) { // not shuffle
    which_line(i) := Mux((U(i, icachecfg.bankIdxWidth bits) < inst0_bankId) && is_crossline12_nxt, U(NL).resized, U(THIS).resized)
  }

  // regs between 12
  val tags_for_match12 = Vec(Vec(RegInit(U(0, icachecfg.tagWidth bits)), icachecfg.wayNum), icachecfg.portNum) 
  when (!stall_12) {
    tags_for_match12 := tags_for_match
  }
  val valids12 = Vec(RegInit(U(0, icachecfg.validRamWordWidth bits)), icachecfg.portNum)
  when (!stall_12) {
    valids12 := valids
  }
  // val v_indexes12 = Vec(RegNextWhen(v_indexes, !stall_12), icachecfg.portNum)
  val is_crossline12 = RegNextWhen(is_crossline12_nxt, !stall_12) init(False)
  val offset12 = RegNextWhen(offset12_nxt, !stall_12) init(0)
  val which_bank12 = RegNextWhen(which_bank, !stall_12)
  val which_output_port12 = RegNextWhen(which_output_port, !stall_12) 
  // val which_line12 = RegInit(Vec(U(0, icachecfg.portIdxWidth bits), icachecfg.bankNum)) 
  val which_line12_noshuffle = RegInit(Vec(U(0, icachecfg.portIdxWidth bits), icachecfg.bankNum)) 
  when (!stall_12) {
    for(i <- 0 until icachecfg.bankNum) { // shuffle & no_shuffle
      // which_line12(i) := which_line(U(i, icachecfg.bankIdxWidth bits) + inst0_bankId)
      which_line12_noshuffle(i) := which_line(i)
    }
  }
  // cache inst info
  val cache_inst_info12 = RegNextWhen(io.cache_inst_info, !stall_12) init(CacheInstInfo.emptyCacheInstInfo)
  val tag_for_index_store12 = RegNextWhen(io.tag_for_index_store, !stall_12) init(U(0, icachecfg.tagWidth bits))
  val idx_for_index_store12 = RegNextWhen(getIdxForIndexStore(io.fetch_if.vaddr), !stall_12) init(U(0, icachecfg.idxWidth bits))
  // stage 2

  // random replace
  val counter = RegInit(U(0, icachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idxes = Vec(UInt(icachecfg.idxWidth bits), icachecfg.portNum)
  for (i <- 0 until icachecfg.portNum) {
    victim_idxes(i) := counter
    when (!valids12(i).andR) {
      for (j <- 0 until icachecfg.wayNum) {
        when (valids12(i)(j) === False) {
          victim_idxes(i) := U(j).resize(icachecfg.idxWidth)
        }
      }
    }
  }
  val victim_idxes12 = Vec(RegInit(U(0, icachecfg.idxWidth bits)), icachecfg.portNum) // this reg'll be modified in mshr_fsm
  // hit logic
  val ptags = Vec(UInt(icachecfg.tagWidth bits), icachecfg.portNum)
  ptags(THIS) := getPTag(paddrs(THIS))
  ptags(NL) := getNLPTag(paddrs(THIS), v_indexes12(NL))
  
  // is hit ? paddr valid and missdata loaded used later, not now
  val hit_bits = Vec(UInt(icachecfg.wayNum bits), icachecfg.portNum)
  for (i <- 0 until icachecfg.portNum) {
    for(j <- 0 until icachecfg.wayNum) {
      hit_bits(i)(j) := valids12(i)(j) && (tags_for_match12(i)(j) === ptags(i))
    }
  }
  val is_hits = hit_bits.map(x => x.orR)
  val fsm_to_hits = Vec(Bool(), icachecfg.portNum)
  val fsm_to_misses = Vec(Bool(), icachecfg.portNum) // actual miss
  for (i <- 0 until icachecfg.portNum) {
    fsm_to_hits(i) := is_hits(i) && paddr_valid
    fsm_to_misses(i) := ~is_hits(i) && paddr_valid
  }
  val selected_idxes = hit_bits.map(x => OHToUInt(x).resize(icachecfg.idxWidth))

  // data read
  val inst_pkg = Vec(UInt(32 bits), icachecfg.bankNum)

  // reg 2 - 3
  // val offset23 = RegNextWhen(offset12, !stall_23) 
  val which_bank23 = RegNextWhen(which_bank12, !stall_23)
  // val which_line23 = RegNextWhen(which_line12, !stall_23)
  val fsm_to_hit23 = RegInit(False)
  when (!paddr_valid || cache_inst_info12.isCacheInst) { // icache inst then icache mustn't hit
    fsm_to_hit23 := False
  }.elsewhen (!stall_23) {
    // hit signal in stage 2
    fsm_to_hit23 := fsm_to_hits(THIS) && !is_crossline12 || fsm_to_hits.andR && is_crossline12 || is_refill 
  }

  // stage 3
  for (i <- 0 until icachecfg.bankNum) {
    io.fetch_if.insts(i) := inst_pkg(which_bank23(i))
  }

  // genenate miss_addr (for cache)
  val cache_miss_addrs = Vec(UInt(icachecfg.dataAddrWidth bits), icachecfg.portNum)
  val miss_offset = RegInit(U(0, icachecfg.offsetWidth bits))
  for (i <- 0 until icachecfg.portNum) {
    cache_miss_addrs(i) := v_indexes12(i) @@ victim_idxes12(i) @@ getBankOffset(miss_offset)
  }
  // fsm, req data from stage 2
  val miss_fsm = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        when (cache_inst_info12.isCacheInst) {
          when(cache_inst_info12.opcode(1)) { victim_idxes12(THIS) := idx_for_index_store12 }
          .otherwise { victim_idxes12(THIS) := selected_idxes(THIS) }
          stall_12 := True
          goto(INVALIDATE)
        }.elsewhen (fsm_to_misses(THIS)) {
          victim_idxes12(THIS) := victim_idxes(THIS)
          miss_offset := U(0)
          stall_12 := True
          goto(LOAD0)
        }.elsewhen (fsm_to_misses(NL) && is_crossline12) {
          victim_idxes12(NL) := victim_idxes(NL)
          miss_offset := U(0)
          stall_12 := True
          goto(LOAD1)
        }
      }
    } // IDLE end
    val LOAD0: State = new State {
      whenIsActive {
        // make a creq 
        io.creq.valid := True
        io.creq.is_write := False
        io.creq.size := CReq.MSIZE4
        io.creq.addr := paddrs(THIS)(31 downto icachecfg.indexLowerBound) @@ U(0, icachecfg.offsetUpperBound+1 bits)
        io.creq.strobe := U(0)
        io.creq.data := U(0)
        io.creq.burst := CReq.AXI_BURST_INCR
        io.creq.len := CReq.MLEN16

        stall_12 := True
        when (io.cresp.ready) {
          miss_offset := miss_offset + 1
        }
        when (io.cresp.last) {
          when (is_crossline12 && fsm_to_misses(NL)) {
            miss_offset := U(0)
            victim_idxes12(NL) := victim_idxes(NL)
            goto(LOAD1)
          }.otherwise {
            goto(REFILL)
          }
        } 
      }// when is active block end
    } // LOAD0 end
    val LOAD1: State = new State {
      whenIsActive {
        // make a creq 
        io.creq.valid := True
        io.creq.is_write := False
        io.creq.size := CReq.MSIZE4
        io.creq.addr := paddrs(NL)(31 downto icachecfg.indexLowerBound) @@ U(0, icachecfg.offsetUpperBound+1 bits)
        io.creq.strobe := U(0)
        io.creq.data := U(0)
        io.creq.burst := CReq.AXI_BURST_INCR
        io.creq.len := CReq.MLEN16

        stall_12 := True
        when (io.cresp.ready) {
          miss_offset := miss_offset + 1
        }
        when (io.cresp.last) {
          goto(REFILL)
        } 
      }// when is active block end
    } // LOAD1 end
    val INVALIDATE: State = new State {
      whenIsActive {
        stall_12 := True
        goto(REFILL)
      }
    } // INVALIDATE end
    val REFILL: State = new State {
      whenIsActive {
        stall_12 := True
        when (vaddr_valid) { // it's actually means no new req, so hold current status
          stall_12 := False
          goto(IDLE)
        }
      }// when is active block end
    } // REFILL end
  }

  // has_fsm_loadings
  has_fsm_loadings(THIS) := miss_fsm.isActive(miss_fsm.LOAD0)
  has_fsm_loadings(NL) := miss_fsm.isActive(miss_fsm.LOAD1)
  is_refill := miss_fsm.isActive(miss_fsm.REFILL)
  // read/write dataRam
  val dataRam_actual_pkg = Vec(InstRamPort(), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {
    // addr
    when (has_fsm_loadings(THIS)) {
      dataRam_actual_pkg(i).addr := cache_miss_addrs(THIS)
    }.elsewhen (has_fsm_loadings(NL)) {
      dataRam_actual_pkg(i).addr := cache_miss_addrs(NL)
    }.elsewhen (is_refill && fsm_to_misses(THIS) && which_line12_noshuffle(i) === THIS) {
      dataRam_actual_pkg(i).addr := v_indexes12(THIS) @@ victim_idxes12(THIS) @@ getBankOffset(offset12 + which_output_port12(i))
    }.elsewhen (is_refill && fsm_to_misses(NL) && which_line12_noshuffle(i) === NL) { // !crossline => we don't care NL rdata 
      dataRam_actual_pkg(i).addr := v_indexes12(NL) @@ victim_idxes12(NL) @@ U(0, icachecfg.bankOffsetWidth bits)
    }.elsewhen (which_line12_noshuffle(i) === THIS) {
      dataRam_actual_pkg(i).addr := v_indexes12(THIS) @@ selected_idxes(THIS) @@ getBankOffset(offset12 + which_output_port12(i))
    }.otherwise {
      dataRam_actual_pkg(i).addr := v_indexes12(NL) @@ selected_idxes(NL) @@ U(0, icachecfg.bankOffsetWidth bits)
    }
    // write
    dataRam_actual_pkg(i).write := getBankId(miss_offset) === U(i, icachecfg.bankIdxWidth bits) && has_fsm_loadings.orR
    // data
    dataRam_actual_pkg(i).data := io.cresp.data
  }
    
  for (i <- 0 until icachecfg.bankNum) {
    inst_pkg(i) := dataRam(i).readWriteSync(address=dataRam_actual_pkg(i).addr, data=dataRam_actual_pkg(i).data, 
                    enable=dataRam_actual_pkg(i).enable, write=dataRam_actual_pkg(i).write,
                    mask=dataRam_actual_pkg(i).mask)
  }

  // write meta ram
  val port_chosen = whichPortLoading(has_fsm_loadings)
  val tagRam_refill_data = UInt(icachecfg.tagRamWordWidth bits)
  val validRam_refill_data = UInt(icachecfg.validRamWordWidth bits)
  val victim_idx_for_refill = UInt(icachecfg.idxWidth bits)
  // modified for cache insts
  val ptag_for_refill = Mux(cache_inst_info12.isCacheInst && cache_inst_info12.opcode === CacheInstInfo.INDEX_STORE, tag_for_index_store12, ptags(port_chosen))
  val valid_for_refill = !cache_inst_info12.isCacheInst
  tagRam_refill_data := (tags_for_match12(port_chosen).asBits).asUInt
  validRam_refill_data := valids12(port_chosen)
  victim_idx_for_refill := victim_idxes12(port_chosen)
  switch(victim_idx_for_refill) {
    is(U(1)) {
      tagRam_refill_data(icachecfg.tagWidth * 2 - 1 downto icachecfg.tagWidth) := ptag_for_refill
      validRam_refill_data(1) := valid_for_refill
    }
    if(icachecfg.wayNum >= 4) {
    is(U(2)) {
      tagRam_refill_data(icachecfg.tagWidth * 3 - 1 downto icachecfg.tagWidth * 2) := ptag_for_refill
      validRam_refill_data(2) := valid_for_refill
    }
    is(U(3)) {
      tagRam_refill_data(icachecfg.tagWidth * 4 - 1 downto icachecfg.tagWidth * 3) := ptag_for_refill
      validRam_refill_data(3) := valid_for_refill
    }
    if(icachecfg.wayNum >= 8) {
    is(U(4)) {
      tagRam_refill_data(icachecfg.tagWidth * 5 - 1 downto icachecfg.tagWidth * 4) := ptag_for_refill
      validRam_refill_data(4) := valid_for_refill
    }
    is(U(5)) {
      tagRam_refill_data(icachecfg.tagWidth * 6 - 1 downto icachecfg.tagWidth * 5) := ptag_for_refill
      validRam_refill_data(5) := valid_for_refill
    }
    is(U(6)) {
      tagRam_refill_data(icachecfg.tagWidth * 7 - 1 downto icachecfg.tagWidth * 6) := ptag_for_refill
      validRam_refill_data(6) := valid_for_refill
    }
    is(U(7)) {
      tagRam_refill_data(icachecfg.tagWidth * 8 - 1 downto icachecfg.tagWidth * 7) := ptag_for_refill
      validRam_refill_data(7) := valid_for_refill
    }
    } 
    }// if(icachecfg.wayNum >= 4) block end
    default {
      tagRam_refill_data(icachecfg.tagWidth-1 downto 0) := ptag_for_refill
      validRam_refill_data(0) := valid_for_refill
    }
  }
  tagRam.map(x => x.write(address=v_indexes12(port_chosen), data=tagRam_refill_data, 
      enable=(io.cresp.last || miss_fsm.isActive(miss_fsm.INVALIDATE)))) // CACHE inst make cacheLine invalid so don't mind we write sth. in
  // INDEX_STORE never mention valid bits, so we'd better not change them
  when (io.cresp.last || miss_fsm.isActive(miss_fsm.INVALIDATE)) {
    validRam(v_indexes12(port_chosen)) := validRam_refill_data
  }

  // ctl unit
  when (!vaddr_valid) {
    stall_12 := True
    // stall_23 := True
  }
  // output bit
  io.fetch_if.hit := (fsm_to_hits(THIS) && !is_crossline12 || fsm_to_hits.andR && is_crossline12) && !cache_inst_info12.isCacheInst || is_refill // hit signal is in stage 2
  io.fetch_if.instValid := fsm_to_hit23

  // utils
  def getBankId(offset: UInt): UInt = offset(icachecfg.bankIdxWidth-1 downto 0)
  def getBankOffset(offset: UInt): UInt = offset(icachecfg.offsetWidth-1 downto icachecfg.bankIdxWidth)
  def getBankAddr(index: UInt, idx_way: UInt, bank_offset: UInt): UInt = index @@ idx_way @@ bank_offset
  def getVIndex(vaddr: UInt): UInt = vaddr(icachecfg.indexUpperBound downto icachecfg.indexLowerBound)
  def getPTag(paddr: UInt): UInt = paddr(icachecfg.tagUpperBound downto icachecfg.tagHiLowerBound) @@ paddr(icachecfg.tagLoUpperBound downto icachecfg.tagLowerBound)
  def getNLPTag(this_paddr: UInt, v_index: UInt): UInt = {
    val ret = UInt(icachecfg.tagWidth bits)
    when (v_index === U(0)) {
      ret := this_paddr(icachecfg.tagUpperBound downto icachecfg.tagHiLowerBound) @@ (this_paddr(icachecfg.tagLoUpperBound downto icachecfg.tagLowerBound) + U(1))
    }.otherwise {
      ret := getPTag(this_paddr)
    }
    ret
  }
  def getNLPAddr(this_paddr: UInt, v_index: UInt): UInt = {
    val ret = UInt(32 bits)
    when (v_index === U(0)) {
      ret := this_paddr(31 downto icachecfg.tagLoUpperBound+1) @@ (this_paddr(icachecfg.tagLoUpperBound downto icachecfg.tagLowerBound) + U(1)) @@ U(0, icachecfg.tagLowerBound bits)
    }.otherwise {
      ret := this_paddr(31 downto icachecfg.tagLowerBound) @@ v_index @@ U(0, icachecfg.indexLowerBound bits)
    }
    ret
  }
  def whichPortLoading(has_fsm_loadings: Vec[Bool]): UInt = {
    val ret = UInt(icachecfg.portIdxWidth bits)
    when(has_fsm_loadings(NL)) {
      ret := U"1"
    }.otherwise {
      ret := U"0"
    }
    ret
  }
  def getIdxForIndexStore(vaddr: UInt) = vaddr(icachecfg.indexUpperBound+icachecfg.idxWidth downto icachecfg.indexUpperBound+1)

  def THIS = 0 //U"0"
  def NL = 1 //U"1"
}

object ICache {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new ICache)
  }
}