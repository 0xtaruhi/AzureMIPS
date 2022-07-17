package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core.ifu.IF2ICache
import azuremips.core._

case class ICache(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val fetch_if = slave(IF2ICache(config))
    val cresp = in(new CResp())
    val creq = out(new CReq())
  }
  
  // some rename
  val icachecfg = config.icache
  val vaddr = io.fetch_if.vaddr
  val vaddr_valid = io.fetch_if.vaddr_valid
  val paddr = io.fetch_if.paddr
  val paddr_valid = io.fetch_if.paddr_valid
  val stall_12 = False
  val stall_23 = False
  val is_refill = Bool()
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
  val tagRam = Mem(UInt(icachecfg.tagRamWordWidth bits), icachecfg.setNum)
  // meta, i.e. valid ram
  val validRam = Vec(RegInit(U(0, icachecfg.validRamWordWidth bits)), icachecfg.setNum)
  // val validRam_nxt = Vec(UInt(icachecfg.validRamWordWidth bits), icachecfg.setNum)
  // validRam_nxt := validRam
  // data ram, banks yield
  val dataRam = for (i <- 0 until icachecfg.bankNum) yield {
    Mem(UInt(icachecfg.dataRamWordWidth bits), icachecfg.bankSize)
  }
  // stage 1
  val v_index = vaddr(icachecfg.indexUpperBound downto icachecfg.indexLowerBound)
  val tags = tagRam.readAsync(address=v_index)
  val valids = validRam(v_index)
  val tags_for_match = Vec(UInt(icachecfg.tagWidth bits), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {
    tags_for_match(i) := tags(icachecfg.tagWidth*(i+1) - 1 downto icachecfg.tagWidth*i)
  }

  // random replace
  val counter = RegInit(U(0, icachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idx = UInt(icachecfg.idxWidth bits)
  victim_idx := counter
  when (!valids.andR) {
    for (i <- 0 until icachecfg.wayNum) {
      when (valids(i) === False) {
        victim_idx := U(i).resize(icachecfg.idxWidth)
      }
    }
  }
  // shuffle
  val offsets = Vec(UInt(icachecfg.offsetWidth bits), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {
    offsets(i) := (vaddr(icachecfg.offsetUpperBound downto icachecfg.offsetLowerBound) + i).resized
  }
  val data_ren = Vec(Bool(), icachecfg.bankNum)
  val data_ren12_nxt = Vec(Bool(), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {
    data_ren(i) := offsets(i) >= offsets(0)
  }
  val offsets_shuffled = Vec(UInt(icachecfg.offsetWidth bits), icachecfg.bankNum)
  switch(getBankId(offsets(0))){
    is(U(1)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(i) := offsets(U(i+3).resize(icachecfg.bankIdxWidth))
        data_ren12_nxt(i) := data_ren(U(i+3).resize(icachecfg.bankIdxWidth))
      }
    }
    is(U(2)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(i) := offsets(U(i+2).resize(icachecfg.bankIdxWidth))
        data_ren12_nxt(i) := data_ren(U(i+2).resize(icachecfg.bankIdxWidth))
      }
    }
    is(U(3)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(i) := offsets(U(i+1).resize(icachecfg.bankIdxWidth))
        data_ren12_nxt(i) := data_ren(U(i+1).resize(icachecfg.bankIdxWidth))
      }
    }
    default {
      offsets_shuffled := offsets
      data_ren12_nxt := data_ren
    }
  }

  // regs between 12
  val tags_for_match12 = Vec(Reg(UInt(icachecfg.tagWidth bits)), icachecfg.bankNum)
  when (!stall_12) {
    tags_for_match12 := tags_for_match
  }
  val valids12 = Reg(UInt(icachecfg.validRamWordWidth bits))
  when (!vaddr_valid) {
    valids12 := U(0)
  }.elsewhen (!stall_12) {
    valids12 := valids
  }
  val data_ren12 = Vec(Reg(Bool()), icachecfg.bankNum)
  when (!vaddr_valid) {
    data_ren12 := Vec(False, icachecfg.bankNum)
  }.elsewhen (!stall_12) {
    data_ren12 := data_ren12_nxt
  }
  val v_index12 = RegInit(U(0, icachecfg.indexWidth bits))
  when (!stall_12) {
    v_index12 := v_index
  }
  val offsets_shuffled12 = Vec(RegInit(U(0, icachecfg.offsetWidth bits)), icachecfg.bankNum)
  when (!stall_12) {
    offsets_shuffled12 := offsets_shuffled
  }
  val victim_idx12 = RegInit(U(0, icachecfg.idxWidth bits))
  when (!stall_12) {
    victim_idx12 := victim_idx
  }

  // stage 2
  val ptag = paddr(icachecfg.tagUpperBound downto icachecfg.tagLowerBound)
  val poffset = paddr(icachecfg.offsetUpperBound downto icachecfg.offsetLowerBound)
  
  // is hit ? paddr valid and missdata loaded used later, not now
  val hit_bits = UInt(icachecfg.wayNum bits)
  for(i <- 0 until icachecfg.wayNum) {
    hit_bits(i) := valids12(i) && (tags_for_match12(i) === ptag)
  }
  val is_hit = hit_bits.orR
  val fsm_to_hit = paddr_valid && is_hit // define it here to emphasize: fsm_to_xxx is a stage 2 signal
  // val fsm_to_miss = paddr_valid && !is_hit // fsm_to_miss doesn't mind the fsm's state
  val selected_idx = OHToUInt(hit_bits).resize(icachecfg.idxWidth)
  // hit, then gen read en for each bank, aka inst valids before reorder
  val data_ren23_nxt = Vec(Bool(), icachecfg.bankNum)
  (data_ren23_nxt zip data_ren12).foreach{case(ren_nxt, ren12) => ren_nxt := ren12 && paddr_valid}

  // data read
  val inst_pkg = Vec(UInt(32 bits), icachecfg.bankNum)
  val cache_addrs = Vec(UInt(icachecfg.dataAddrWidth bits), icachecfg.bankNum)
  for(i <- 0 until icachecfg.bankNum) {
    cache_addrs(i) := Mux(is_refill, getBankAddr(v_index12, victim_idx12, getBankOffset(offsets_shuffled12(i))), getBankAddr(v_index12, selected_idx, getBankOffset(offsets_shuffled12(i)))) 
    inst_pkg(i) := dataRam(i).readSync(address=cache_addrs(i)) // , readUnderWrite=writeFirst)
  }

  // reg 2 - 3
  val data_ren23 = Vec(RegInit(False), icachecfg.bankNum)
  when (!paddr_valid) {
    data_ren23 := Vec(False, icachecfg.bankNum)
  }.elsewhen (!stall_23) {
    data_ren23 := data_ren23_nxt
  }
  val poffset23 = RegNext(poffset) 

  // stage 3
  val instValids_shuffled = Vec(Bool(), icachecfg.bankNum)
  switch (getBankId(poffset23)) {
    is(U(1)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(i) := inst_pkg(U(i+1).resize(icachecfg.bankIdxWidth))
        instValids_shuffled(i) := data_ren23(U(i+1).resize(icachecfg.bankIdxWidth))
      }
    }
    is(U(2)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(i) := inst_pkg(U(i+2).resize(icachecfg.bankIdxWidth))
        instValids_shuffled(i) := data_ren23(U(i+2).resize(icachecfg.bankIdxWidth))
      }
    }
    is(U(3)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(i) := inst_pkg(U(i+3).resize(icachecfg.bankIdxWidth))
        instValids_shuffled(i) := data_ren23(U(i+3).resize(icachecfg.bankIdxWidth))
      }
    }
    default {
      io.fetch_if.insts := inst_pkg
      instValids_shuffled := data_ren23
    }
  }

  // genenate miss_addr (for cache)
  val miss_addr_offset = RegInit(U(0, icachecfg.offsetWidth bits))
  
  // fsm, req data from stage 2
  val miss_fsm = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        when (paddr_valid && !is_hit) {
          miss_addr_offset := U(0) // poffset // in stage 2!
          goto(LOAD)
        }
      }
      onExit(stall_12 := True)
    } // IDLE end
    val LOAD: State = new State {
      whenIsActive {
        // make a creq 
        io.creq.valid := True
        io.creq.is_write := False
        io.creq.size := CReq.MSIZE4
        io.creq.addr := paddr(31 downto icachecfg.indexLowerBound) @@ U(0, icachecfg.offsetUpperBound+1 bits)
        io.creq.strobe := U(0)
        io.creq.data := U(0)
        io.creq.burst := CReq.AXI_BURST_INCR
        io.creq.len := CReq.MLEN16

        stall_12 := True
        when (io.cresp.ready) {
          miss_addr_offset := miss_addr_offset + 1
        }
        when (io.cresp.last) {
          goto(REFILL)
        } 
      }// when is active block end
    } // LOAD end
    val REFILL: State = new State {
      whenIsActive {
        goto(IDLE)
      }// when is active block end
    } // REFILL end
  }

  // write dataRam
  val load_wen = Vec(Bool(), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {
    load_wen(i) := getBankId(miss_addr_offset) === U(i, icachecfg.bankIdxWidth bits) && miss_fsm.isActive(miss_fsm.LOAD)
  }
  for (i <- 0 until icachecfg.bankNum) {
    val miss_dataram_addr = getBankAddr(v_index12, victim_idx12, getBankOffset(miss_addr_offset))
    dataRam(i).write(address=miss_dataram_addr, data=io.cresp.data, enable=load_wen(i))
  }
  // write meta ram
  val tagRam_refill_data = UInt(icachecfg.tagRamWordWidth bits)
  val validRam_refill_data = UInt(icachecfg.validRamWordWidth bits)
  tagRam_refill_data := (tags_for_match12.asBits).asUInt
  validRam_refill_data := valids12
  switch(victim_idx12) {
    is(U(1)) {
      tagRam_refill_data(icachecfg.tagWidth * 2 - 1 downto icachecfg.tagWidth) := ptag
      validRam_refill_data(1) := True
    }
    if(icachecfg.wayNum == 4) {
    is(U(2)) {
      tagRam_refill_data(icachecfg.tagWidth * 3 - 1 downto icachecfg.tagWidth * 2) := ptag
      validRam_refill_data(2) := True
    }
    is(U(3)) {
      tagRam_refill_data(icachecfg.tagWidth * 4 - 1 downto icachecfg.tagWidth * 3) := ptag
      validRam_refill_data(3) := True
    }
    } // if(icachecfg.wayNum == 4) block end
    default {
      tagRam_refill_data(icachecfg.tagWidth-1 downto 0) := ptag
      validRam_refill_data(0) := True
    }
  }
  tagRam.write(address=v_index12, data=tagRam_refill_data, enable=io.cresp.last)// , mask=meta_write_mask)
  when (io.cresp.last) {
    validRam(v_index12) := validRam_refill_data
  }

  // ctl unit
  is_refill := miss_fsm.isActive(miss_fsm.REFILL)
  val fsm_to_hit23 = RegInit(False)
  when (!paddr_valid) {
    fsm_to_hit23 := False
  }.elsewhen (!stall_23) {
    fsm_to_hit23 := fsm_to_hit || is_refill
  }
  // output
  io.fetch_if.hit := fsm_to_hit || is_refill // hit signal is in stage 2
  for(i <- 0 until icachecfg.bankNum) {
    io.fetch_if.instValids(i) := instValids_shuffled(i) && fsm_to_hit23
  }

  // utils
  def getBankId(offset: UInt): UInt = offset(icachecfg.bankIdxWidth-1 downto 0)
  def getBankOffset(offset: UInt): UInt = offset(icachecfg.offsetWidth-1 downto icachecfg.bankIdxWidth)
  def getBankAddr(index: UInt, idx_way: UInt, bank_offset: UInt): UInt = index @@ idx_way @@ bank_offset
  def getVictimIdxRand(valids: UInt, rand: UInt): UInt = {
    val ret = rand.resize(icachecfg.idxWidth)
    when (!valids12.andR) {
      for (i <- 0 until icachecfg.wayNum) {
        when (valids(i) === False) {
          ret := U(i).resize(icachecfg.idxWidth)
        }
      }
    }
    ret
  }
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