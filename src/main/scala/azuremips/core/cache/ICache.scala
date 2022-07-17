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
  val stall_12 = Bool()
  val stall_23 = Bool()
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
  val validRam = Mem(UInt(icachecfg.validRamWordWidth bits), icachecfg.setNum)
  // data ram, banks yield
  val dataRam = for (i <- 0 until icachecfg.bankNum) yield {
    Mem(UInt(icachecfg.dataRamWordWidth bits), icachecfg.bankSize)
  }
  // stage 1
  val v_index = vaddr(icachecfg.indexUpperBound downto icachecfg.indexLowerBound)
  val tags = tagRam.readAsync(address=v_index, readUnderWrite=writeFirst)
  val valids = validRam.readAsync(address=v_index, readUnderWrite=writeFirst)

  // regs between 12
  val tags12 = Reg(UInt(icachecfg.tagRamWordWidth bits))
  when (!vaddr_valid) {
    tags12 := U(0)
  }.elsewhen (!stall_12) {
    tags12 := tags
  }
  val valids12 = Reg(UInt(icachecfg.validRamWordWidth bits))
  when (!vaddr_valid) {
    valids12 := U(0)
  }.elsewhen (!stall_12) {
    valids12 := valids
  }
  val vaddr_valid12 = RegInit(False)
  when (!stall_12) {
    vaddr_valid12 := vaddr_valid
  }
  val v_index12 = RegInit(U(0, icachecfg.indexWidth bits))
  when (!stall_12) {
    v_index12 := v_index
  }

  // stage 2
  val ptag = paddr(icachecfg.tagUpperBound downto icachecfg.tagLowerBound)
  val poffset = paddr(icachecfg.offsetUpperBound downto icachecfg.offsetLowerBound)
  
  val offsets = Vec(UInt(icachecfg.offsetWidth bits), icachecfg.bankNum)
  for(i <- 0 until icachecfg.bankNum) {
    offsets(i) := (poffset + i)
  }
  // shuffle offsets
  val offsets_shuffled = Vec(UInt(icachecfg.offsetWidth bits), icachecfg.bankNum)
  offsets_shuffled := offsets
  switch(getBankId(offsets(0))){
    is(U(1)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(U(icachecfg.bankNum + i - 1).resize(icachecfg.bankIdxWidth)) := offsets(i)
      }
    }
    is(U(2)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(U(icachecfg.bankNum + i - 2).resize(icachecfg.bankIdxWidth)) := offsets(i)
      }
    }
    is(U(3)) {
      for (i <- 0 until icachecfg.bankNum) {
        offsets_shuffled(U(icachecfg.bankNum + i - 3).resize(icachecfg.bankIdxWidth)) := offsets(i)
      }
    }
    default {
      offsets_shuffled := offsets
    }
  }
  // is hit ? paddr valid and missdata loaded used later, not now
  val hit_bits = UInt(icachecfg.wayNum bits)
  for(i <- 0 until icachecfg.wayNum) {
    hit_bits(i) := Mux(valids(i) && (tags12(icachecfg.tagWidth + i - 1 downto i) === ptag), True, False)
  }
  val is_hit = hit_bits.orR
  val fsm_to_hit = Bool() // define it here to emphasize: fsm_to_xxx is a stage 2 signal
  val fsm_to_miss = Bool()
  val selected_idx = OHToUInt(hit_bits).resize(icachecfg.idxWidth)
  // hit, then gen read en for each bank, aka inst valids before reorder
  val data_ren = Vec(Bool(), icachecfg.bankNum)
  for (i <- 0 until icachecfg.bankNum) {// not the same cache line, ren = False
    data_ren(i) := paddr_valid && (offsets(i) >= offsets(0))
  }
  // random replace
  val counter = RegInit(U(0, icachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idx = getVictimIdxRand(valids12, counter)

  // data read
  val inst_pkg = Vec(UInt(32 bits), icachecfg.bankNum)
  val cache_addrs = Vec(UInt(icachecfg.dataAddrWidth bits), icachecfg.bankNum)
  for(i <- 0 until icachecfg.bankNum) {
    cache_addrs(i) := getBankAddr(v_index12, selected_idx, getBankOffset(offsets_shuffled(i)))
    inst_pkg(i) := dataRam(i).readSync(address=cache_addrs(i), enable=data_ren(i), readUnderWrite=writeFirst)
  }
  // reg 2 - 3
  val fsm_to_hit23 = RegInit(False)
  when (!paddr_valid) {
    fsm_to_hit23 := False
  }.elsewhen (!stall_23) {
    fsm_to_hit23 := fsm_to_hit
  }
  val offsets23 = Vec.fill(icachecfg.bankNum)(RegInit(U(0, icachecfg.offsetWidth bits)))
  when (!paddr_valid) {
    offsets23 := Vec(U(0), icachecfg.bankNum)
  }.elsewhen (!stall_23) {
    offsets23 := offsets
  }
  val inst_valid23 = Vec.fill(icachecfg.bankNum)(RegInit(False))
  when (!paddr_valid) {
    inst_valid23 := Vec(False, icachecfg.bankNum)
  }.elsewhen(!stall_23) {
    inst_valid23 := data_ren
  }
  // v_index for load
  val miss_addr_idx = RegInit(U(0, icachecfg.idxWidth bits))
  when (!paddr_valid) {
    miss_addr_idx := U(0)
  } // there are changes for creq_addr reg below 
  // paddr regester for load
  val creq_addr = Reg(UInt(32 bits))
  when (!paddr_valid) {
    creq_addr := U(0)
  } // there are changes for creq_addr reg below 

  // stage 3
  val instValids_shuffled = Vec(Bool(), icachecfg.bankNum)
  io.fetch_if.insts := inst_pkg
  instValids_shuffled := inst_valid23
  switch (getBankId(offsets23(0))) {
    is(U(1)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(U(icachecfg.bankNum + i - 1).resize(icachecfg.bankIdxWidth)) := inst_pkg(i)
        instValids_shuffled(U(icachecfg.bankNum + i - 1).resize(icachecfg.bankIdxWidth)) := inst_valid23(i)
      }
    }
    is(U(2)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(U(icachecfg.bankNum + i - 2).resize(icachecfg.bankIdxWidth)) := inst_pkg(i)
        instValids_shuffled(U(icachecfg.bankNum + i - 2).resize(icachecfg.bankIdxWidth)) := inst_valid23(i)
      }
    }
    is(U(3)) {
      for (i <- 0 until icachecfg.bankNum) {
        io.fetch_if.insts(U(icachecfg.bankNum + i - 3).resize(icachecfg.bankIdxWidth)) := inst_pkg(i)
        instValids_shuffled(U(icachecfg.bankNum + i - 3).resize(icachecfg.bankIdxWidth)) := inst_valid23(i)
      }
    }
    default {
      io.fetch_if.insts := inst_pkg
      instValids_shuffled := inst_valid23
    }
  }
  for(i <- 0 until icachecfg.bankNum) {
    io.fetch_if.instValids(i) := instValids_shuffled(i) && fsm_to_hit23
  }

  // genenate miss_addr (for cache)
  val miss_addr_offset = RegInit(U(0, icachecfg.offsetWidth bits))
  
  // fsm
  val miss_fsm = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        io.creq.valid := False
        when (fsm_to_miss) {
          creq_addr := paddr // in stage 2!
          miss_addr_offset := U(0) // poffset // in stage 2!
          goto(LOAD)
        }
      }
    }
    val LOAD: State = new State {
      whenIsActive {
        // make a creq 
        io.creq.valid := True
        io.creq.is_write := False
        io.creq.size := CReq.MSIZE4
        io.creq.addr := ((31 downto icachecfg.indexLowerBound) -> paddr(31 downto icachecfg.indexLowerBound), (icachecfg.offsetUpperBound downto 0) -> U(0))
        io.creq.strobe := U(0)
        io.creq.data := U(0)
        io.creq.burst := CReq.AXI_BURST_INCR
        io.creq.len := CReq.MLEN16

        when (io.cresp.ready) {
          val load_wen = Vec(Bool(), icachecfg.bankNum)
          for (i <- 0 until icachecfg.bankNum) {
            load_wen(i) := Mux(getBankId(miss_addr_offset) === U(i, icachecfg.bankIdxWidth bits), True, False)
          }
          for (i <- 0 until icachecfg.bankNum) {
            val miss_dataram_addr = getBankAddr(v_index12, victim_idx, getBankOffset(miss_addr_offset))
            dataRam(i).write(address=miss_dataram_addr, data=io.cresp.data, enable=load_wen(i))
          }
          miss_addr_offset := miss_addr_offset + 1
        }
        when (io.cresp.last) {
          io.creq.valid := False
          // val tagRam_refill_data = U(0, icachecfg.tagRamWordWidth bits)
          // val validRam_refill_data = U(0, icachecfg.validRamWordWidth bits)
          // val meta_write_mask = B(0, icachecfg.wayNum bits)
          val tagRam_refill_data = tags12
          val validRam_refill_data = valids12
          
          switch(victim_idx) {
            // is(U(0)) {} is implemented in default
            is(U(1)) {
              tagRam_refill_data(icachecfg.tagWidth * 2 - 1 downto icachecfg.tagWidth) := ptag
              validRam_refill_data(1) := True
              // meta_write_mask(1) := True
            }
            if(icachecfg.wayNum == 4) {
            is(U(2)) {
              tagRam_refill_data(icachecfg.tagWidth * 3 - 1 downto icachecfg.tagWidth * 2) := ptag
              validRam_refill_data(2) := True
              // meta_write_mask(2) := True
            }
            is(U(3)) {
              tagRam_refill_data(icachecfg.tagWidth * 4 - 1 downto icachecfg.tagWidth * 3) := ptag
              validRam_refill_data(3) := True
              // meta_write_mask(3) := True
            }
            } // if(icachecfg.wayNum == 4) block end
            default {
              tagRam_refill_data(icachecfg.tagWidth-1 downto 0) := ptag
              validRam_refill_data(0) := True
              // meta_write_mask(0) := True
            }
          }
          // meta (tag / valid ram) refill. put it here to emphasize the squential position of "refill".
          tagRam.write(address=v_index12, data=tagRam_refill_data, enable=io.cresp.last)// , mask=meta_write_mask)
          validRam.write(address=v_index12, data=validRam_refill_data, enable=io.cresp.last)// , mask=meta_write_mask)
          goto(IDLE)
        } 
      }// when is active block end
    }
  }

  // ctl unit
  val fsm_in_miss = (miss_fsm.isActive(miss_fsm.LOAD) && !miss_fsm.isEntering(miss_fsm.IDLE)) || miss_fsm.isEntering(miss_fsm.LOAD) // paddr_valid && vaddr_valid12
  stall_23 := False
  stall_12 := fsm_in_miss
  fsm_to_hit := paddr_valid && vaddr_valid12 && (is_hit || miss_fsm.isEntering(miss_fsm.IDLE)) // hit or cresp.last
  fsm_to_miss := paddr_valid && vaddr_valid12 && miss_fsm.isActive(miss_fsm.IDLE) && !is_hit
  io.fetch_if.hit := fsm_to_hit // hit signal is in stage 2

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