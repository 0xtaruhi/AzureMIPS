package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class DCache(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val dreqs = Vec(in(new DReq()), config.dcache.portNum)
    val dresps = Vec(out(new DResp()), config.dcache.portNum)
    val cresps = Vec(in(new CResp()), config.dcache.portNum)
    val creqs = Vec(out(new CReq()), config.dcache.portNum)
  }
  
  // some rename and defination for early accessing
  val dcachecfg = config.dcache
  val vaddr_valids = Vec(Bool(), dcachecfg.portNum)
  (vaddr_valids zip io.dreqs).foreach {case(vaddr_valid, dreq) => vaddr_valid := dreq.vaddr_valid}
  val paddr_valids = Vec(Bool(), dcachecfg.portNum)
  (paddr_valids zip io.dreqs).foreach {case(paddr_valid, dreq) => paddr_valid := dreq.paddr_valid}
  val has_mshr_loadings = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_wbs = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_refills = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_enterRefills = Vec(Bool(), dcachecfg.portNum)
  val stall_12 = Vec(Bool(), dcachecfg.portNum) 
  val dreq_cut_pkg12 = Vec(Reg(DReqCut()), dcachecfg.portNum)
  val has_copy = Vec(Bool(), dcachecfg.portNum) // stage 2 signal 
  val share_v_index = Vec(Bool(), dcachecfg.portNum) // stage 2 signal
  val meta_refresh_en = True
  stall_12 := Vec(False, dcachecfg.portNum)
  // creq initialisation, otherwise latch
  for (i <- 0 until dcachecfg.portNum) {
    io.creqs(i).valid := False
    io.creqs(i).is_write := False
    io.creqs(i).size := CReq.MSIZE4
    io.creqs(i).addr := U(0)
    io.creqs(i).strobe := U(0)
    io.creqs(i).data := U(0)
    io.creqs(i).burst := CReq.AXI_BURST_INCR
    io.creqs(i).len := CReq.MLEN16
  }
  
  // tag ram
  val tagRam = for (i <- 0 until config.dcache.portNum) yield {
    Mem(UInt(dcachecfg.tagRamWordWidth bits), dcachecfg.setNum)
  }
  // meta, i.e. valid/dirty ram
  val validRam_nxt = Vec(UInt(dcachecfg.validRamWordWidth bits), dcachecfg.setNum)
  val validRam = Vec(RegInit(U(0, dcachecfg.validRamWordWidth bits)), dcachecfg.setNum)
  validRam_nxt := validRam
  when(meta_refresh_en) {
    validRam := validRam_nxt
  }
  val dirtyRam = Vec(RegInit(U(0, dcachecfg.dirtyRamWordWidth bits)), dcachecfg.setNum)
  val dirtyRam_nxt = Vec(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.setNum)
  dirtyRam_nxt := dirtyRam
  when(meta_refresh_en) {
    dirtyRam := dirtyRam_nxt
  }
  // data ram, a 2 wr bram
  val dataRam = Mem(UInt(dcachecfg.dataRamWordWidth bits), dcachecfg.bankSize)
  
  // stage 1
  val v_indexs = Vec(UInt(dcachecfg.indexWidth bits), dcachecfg.portNum)
  (v_indexs zip io.dreqs).foreach {case (v_index, dreq) => v_index := getVIndex(dreq.vaddr)}
  
  // read tags 
  val tags = Vec(UInt(dcachecfg.tagRamWordWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    tags(i) := tagRam(i).readAsync(address=v_indexs(i))
  }
  val tags_for_match = Vec(Vec(UInt(dcachecfg.tagWidth bits), dcachecfg.wayNum), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    for (j <- 0 until dcachecfg.wayNum) {
      tags_for_match(i)(j) := tags(i)(j*dcachecfg.tagWidth+dcachecfg.tagWidth-1 downto j*dcachecfg.tagWidth)
    }
  }
  val valids = Vec(UInt(dcachecfg.validRamWordWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    valids(i) := validRam(v_indexs(i))
  }
  val dirtys = Vec(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.portNum)

  val ptags = Vec(UInt(dcachecfg.tagWidth bits), dcachecfg.portNum)
  (ptags zip io.dreqs).foreach {case (ptag, dreq) => ptag := getPTag(dreq.paddr)}
  // tag match
  val hit_bits = Vec(UInt(dcachecfg.wayNum bits), dcachecfg.portNum)
  // val miss_bits = Vec(UInt(dcachecfg.wayNum bits), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    for(j <- 0 until dcachecfg.wayNum) {
      hit_bits(i)(j) := valids(i)(j) && (tags_for_match(i)(j) === ptags(i))
      // miss_bits(i)(j) := !valids(i)(j) || (tags_for_match(i)(j) =/= ptags(i))
    }
  }
  val selected_idxes = hit_bits.map(x => OHToUInt(x).resize(dcachecfg.idxWidth))
  val is_hits = hit_bits.map(x => x.orR)
  // val fsm_to_hits = Vec(Bool(), dcachecfg.portNum)
  val fsm_to_hits = Vec(Bool(), dcachecfg.portNum)
  val fsm_to_misses = Vec(Bool(), dcachecfg.portNum) // actual miss
  val miss_merge = Vec(Bool(), dcachecfg.portNum) // the tag is the same at the same req
  for (i <- 0 until dcachecfg.portNum) {
    fsm_to_hits(i) := is_hits(i) && paddr_valids(i)
    fsm_to_misses(i) := ~is_hits(i) && paddr_valids(i)
  }
  miss_merge(0) := False
  miss_merge(1) := getPTagIndex(io.dreqs(0).paddr) === getPTagIndex(io.dreqs(1).paddr)
  // for saving the request, we create a dreqcut class vector
  val dreq_cut_pkg = Vec(DReqCut(), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    dreq_cut_pkg(i).paddr := io.dreqs(i).paddr
    dreq_cut_pkg(i).strobe := io.dreqs(i).strobe
    dreq_cut_pkg(i).data := io.dreqs(i).data
  }
  // gen victim idx
  val counter = RegInit(U(0, dcachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idxes = Vec(UInt(dcachecfg.idxWidth bits), dcachecfg.portNum)//  
  // val victim_idxes = valids.map(x => getVictimIdxRand(x, counter))
  
  // regs 12
  val v_index12 = Vec(RegInit(U(0, dcachecfg.indexWidth bits)), dcachecfg.portNum) 
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12(i)) {
      v_index12(i) := v_indexs(i)
    } 
  }
  val tags_for_match12 = Vec(Vec(Reg(UInt(dcachecfg.tagWidth bits)), dcachecfg.wayNum), dcachecfg.portNum) 
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12(i)) {
      tags_for_match12(i) := tags_for_match(i)
    } 
  }
  val selected_idxes12 = Vec(RegInit(U(0, dcachecfg.idxWidth bits)), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12(i)) {
      selected_idxes12(i) := selected_idxes(i)
    } 
  }
  val fsm_to_misses12 = Vec(RegInit(False), dcachecfg.portNum)
  val fsm_to_hits12 = Vec(RegInit(False), dcachecfg.portNum)
  val miss_merge12 = Vec(RegInit(False), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12(i)) {
      fsm_to_misses12(i) := fsm_to_misses(i)
      fsm_to_hits12(i) := fsm_to_hits(i)
      miss_merge12(i) := miss_merge(i)
    } 
  }
  // gen victim_index, actually in stage 2, not 1
  for (i <- 0 until dcachecfg.portNum) {
    victim_idxes(i) := counter
    when (!valids(i).andR) {
      for (j <- 0 until dcachecfg.wayNum) {
        when (valids(i)(j) === False) {
          victim_idxes(i) := U(j).resize(dcachecfg.idxWidth)
        }
      }
    }.elsewhen((fsm_to_hits12(1-i) && counter === selected_idxes12(1-i)) && getPTagIndex(dreq_cut_pkg12(i).paddr) === getPTag(dreq_cut_pkg12(1-i).paddr) ||
    (fsm_to_hits(1-i) && counter === selected_idxes(1-i) && getPTagIndex(dreq_cut_pkg12(i).paddr) === getPTag(dreq_cut_pkg(1-i).paddr)) ) {
      victim_idxes(i)(1) := !selected_idxes12(1-i)(1)
      victim_idxes(i)(0) := !selected_idxes(1-i)(0)
    }
  }
  val victim_idxes12 = Vec(RegInit(U(0, dcachecfg.idxWidth bits)), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!has_mshr_loadings(i) && !has_mshr_wbs(i)) {
      victim_idxes12(i) := victim_idxes(i)
    } 
  }
  // val dirtys12 = Vec(RegInit(U(0, dcachecfg.dirtyRamWordWidth bits)), dcachecfg.portNum) // no stall here
  // for (i <- 0 until dcachecfg.portNum) {
  //     dirtys12(i) := dirtys(i)// valids(i)(victim_idxes(i)) && dirtys(i)(victim_idxes(i))
  // }
  // val dreq_cut_pkg12 = Vec(Reg(DReqCut()), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12(i)) {
      dreq_cut_pkg12(i) := dreq_cut_pkg(i)
    } 
  }

  // stage 2 
  // gen dirty_nxt
  for (i <- 0 until dcachecfg.portNum) {
    when(fsm_to_hits12(i) && dreq_cut_pkg12(i).strobe =/= U(0)) {
      dirtyRam_nxt(v_index12(i))(selected_idxes12(i)) := True
    }
  }
  // gen CACHE addr
  val cache_hit_addrs = Vec(UInt(dcachecfg.dataAddrWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    cache_hit_addrs(i) := v_index12(i) @@ selected_idxes12(i) @@ dreq_cut_pkg12(i).paddr(dcachecfg.offsetUpperBound downto dcachecfg.offsetLowerBound)
  }
  // hit flag output
  for(i <- 0 until dcachecfg.portNum) {
    io.dresps(i).hit := fsm_to_hits12(i) || has_mshr_refills(i) || has_copy(i) && has_mshr_refills(1-i)
  }
  val dataRam_port_pkg = Vec(DataRamPort(), dcachecfg.portNum)
  
  // dataRam read/write data
  val data_pkg = for(i <- 0 until dcachecfg.portNum) yield {
    dataRam.readWriteSync(address=dataRam_port_pkg(i).addr, data=dataRam_port_pkg(i).data,
      enable=dataRam_port_pkg(i).enable, write=dataRam_port_pkg(i).write, mask=dataRam_port_pkg(i).mask)
  }

  // regs 23
  
  // stage 3
  // dataRam output 
  for(i <- 0 until dcachecfg.portNum) {
    io.dresps(i).data := data_pkg(i)
  }

  // MSHR unit
  val mshrs = for(i <- 0 until dcachecfg.portNum) yield {
    new Area {
      val addr = dreq_cut_pkg12(i).paddr
      val addr_before_offset = UInt(AzureConsts.paddrWidth-dcachecfg.offsetUpperBound-1 bits)
      addr_before_offset := getPExceptOffset(dreq_cut_pkg12(i).paddr)
      val v_index = UInt(dcachecfg.indexWidth bits)
      val offset_ld = RegInit(U(0, dcachecfg.offsetWidth bits)) // reg, internal
      val offset = UInt(dcachecfg.offsetWidth bits) // wire, output
      val victim_idx = UInt(dcachecfg.idxWidth bits)
      val victim_tag = UInt(dcachecfg.tagWidth bits)
      val cache_addr_miss = getBankAddr(v_index12(i), victim_idxes12(i), getBankOffset(offset))
      v_index := v_index12(i)
      victim_idx := victim_idxes12(i)
      victim_tag := tags_for_match12(i)(victim_idx)
      offset := U(0) // in case for latch

      val fsm_mshr = new StateMachine {
        val IDLE: State = new State with EntryPoint {
          whenIsActive {
            when (!share_v_index(i) && fsm_to_misses12(i) && !miss_merge12(i)) {
              offset_ld := U(0)
              validRam_nxt(v_index)(victim_idxes(i)) := False // attention, not victim_idxes12
              when (dirtyRam(v_index12(i))(victim_idxes(i))) { // read dirtyRam, use v_index12 && victim_idx, not 12
                goto(WRITEBACK)
              }.otherwise {
                goto(LOAD)
              }
            }.elsewhen (!has_mshr_refills(1-i) && fsm_to_misses12(i) && (has_copy(i) || miss_merge12(i))) {
              goto(WAIT_OTHER_PORT)
            }
          }
          onExit(stall_12(i) := True) // stall 12
        } // IDLE end
        val WRITEBACK: State = new State {
          whenIsActive {
            offset := offset_ld
            stall_12(i) := True

            io.creqs(i).valid := True
            io.creqs(i).is_write := True
            io.creqs(i).size := CReq.MSIZE4
            io.creqs(i).addr := getWritebackPAddr(addr_before_offset, victim_tag, v_index)
            io.creqs(i).strobe := U"1111"
            io.creqs(i).data := data_pkg(i)
            io.creqs(i).burst := CReq.AXI_BURST_INCR
            io.creqs(i).len := CReq.MLEN16
            when (io.cresps(i).ready) {
              offset := offset_ld + 1
              offset_ld := offset_ld + 1
            }
            when(io.cresps(i).last) {
              offset_ld := U(0)
              goto(LOAD)
            }
          }
        } // WRITEBACK end 
        val LOAD: State = new State {
          whenIsActive {
            offset := offset_ld
            stall_12(i) := True

            io.creqs(i).valid := True
            io.creqs(i).is_write := False
            io.creqs(i).size := CReq.MSIZE4
            io.creqs(i).addr := addr_before_offset @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
            io.creqs(i).strobe := U"0000"
            io.creqs(i).data := U(0)
            io.creqs(i).burst := CReq.AXI_BURST_INCR
            io.creqs(i).len := CReq.MLEN16
            when (io.cresps(i).ready) {
              offset_ld := offset_ld + 1
            }
            when(io.cresps(i).last) {
              dirtyRam_nxt(v_index)(victim_idx) := dreq_cut_pkg12(i).strobe =/= U(0) // write dirtyRam
              validRam_nxt(v_index)(victim_idx) := True // write validRam
              goto(REFILL)
            }
          }
        } // LOAD end 
        val REFILL: State = new State { // for waiting tag ram fresh
          whenIsActive {
            goto(IDLE)
          }
        } // REFILL end
        val WAIT_OTHER_PORT: State = new State { // for waiting tag ram fresh
          whenIsActive {
            stall_12(i) := True
            when (has_mshr_refills(1-i)) {
              stall_12(i) := False
              goto(IDLE)
            }
          }
        } // WAIT_OTHER_PORT end
      } // fsm defination
    } // MSHR defination
  } // MSHRs defination
  

  // read state for mshrs.fsm_mshr
  // has_mshr_loading
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_loadings(i) := mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.LOAD)
  }
  // has_mshr_wb
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_wbs(i) := mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.WRITEBACK)
  }
  // has_mshr_refills
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_refills(i) := mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.REFILL)
  }
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_enterRefills(i) := io.cresps(i).last && mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.LOAD)
  }

  // mux before dataRam, stage 2
  // data port
  for(i <- 0 until dcachecfg.portNum) { // the LOAD using dataRam_port
    when (!checkFsmIdle(i, has_mshr_loadings, has_mshr_wbs)) {// MSHR i fsm != WB or LOAD
      dataRam_port_pkg(i).addr := mshrs(i).cache_addr_miss
      dataRam_port_pkg(i).data := io.cresps(i).data
      dataRam_port_pkg(i).mask := Mux(has_mshr_loadings(i), B"1111", B"0000")
      dataRam_port_pkg(i).enable := True
    }.elsewhen (has_mshr_refills(i) || (has_mshr_refills(1-i) && has_copy(i) && fsm_to_misses12(i))) { // this or other port, use fsm_to_miss12. 
      dataRam_port_pkg(i).addr := v_index12(i) @@ victim_idxes12(OHToUInt(has_mshr_refills).resize(dcachecfg.portIdxWidth)) @@ 
                                    dreq_cut_pkg12(i).paddr(dcachecfg.offsetUpperBound downto dcachecfg.offsetLowerBound)
      dataRam_port_pkg(i).data := dreq_cut_pkg12(i).data
      dataRam_port_pkg(i).mask := dreq_cut_pkg12(i).strobe.asBits
      dataRam_port_pkg(i).enable := True
    }.otherwise {
      dataRam_port_pkg(i).addr := cache_hit_addrs(i)
      dataRam_port_pkg(i).data := dreq_cut_pkg12(i).data
      dataRam_port_pkg(i).mask := dreq_cut_pkg12(i).strobe.asBits
      dataRam_port_pkg(i).enable := fsm_to_hits12(i)
    }
  }
  // has_copy generate
  has_copy(0) := getPTagIndex(dreq_cut_pkg12(0).paddr) === getPTagIndex(dreq_cut_pkg12(1).paddr) && !mshrs(1).fsm_mshr.isActive(mshrs(1).fsm_mshr.IDLE)
  has_copy(1) := getPTagIndex(dreq_cut_pkg12(0).paddr) === getPTagIndex(dreq_cut_pkg12(1).paddr) && !mshrs(0).fsm_mshr.isActive(mshrs(0).fsm_mshr.IDLE)
  // share same index generate
  share_v_index(0) := v_index12(0) === v_index12(1) && !mshrs(1).fsm_mshr.isActive(mshrs(1).fsm_mshr.IDLE)
  share_v_index(1) := v_index12(0) === v_index12(1) && !fsm_to_misses12(0) // incase for req at the same time: && !mshrs(0).fsm_mshr.isActive(mshrs(0).fsm_mshr.IDLE)
  for (i <- 0 until dcachecfg.portNum) {
    when (share_v_index(i) && getPTag(dreq_cut_pkg12(0).paddr) =/= getPTag(dreq_cut_pkg12(1).paddr)) {
      stall_12(i) := True // another stall signal gen condition: share v_index && miss
    }
  }
  // write tag 
  val tagRam_refill_data = UInt(dcachecfg.tagRamWordWidth bits)
  
  val mshr_chosen = OHToUInt(has_mshr_enterRefills).resize(dcachecfg.portIdxWidth)
  val victim_idx_for_refill = victim_idxes12(mshr_chosen)
  tagRam_refill_data := (tags_for_match12(mshr_chosen).asBits).asUInt
  if(dcachecfg.wayNum >= 4) {
    switch(victim_idx_for_refill) { // can only be used in 4 way
      is(U(1)) {
        tagRam_refill_data(dcachecfg.tagWidth * 2 - 1 downto dcachecfg.tagWidth) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      is(U(2)) {
        tagRam_refill_data(dcachecfg.tagWidth * 3 - 1 downto dcachecfg.tagWidth * 2) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      is(U(3)) {
        tagRam_refill_data(dcachecfg.tagWidth * 4 - 1 downto dcachecfg.tagWidth * 3) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      if(dcachecfg.wayNum == 8) {
      is(U(4)) {
        tagRam_refill_data(dcachecfg.tagWidth * 5 - 1 downto dcachecfg.tagWidth * 4) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      is(U(5)) {
        tagRam_refill_data(dcachecfg.tagWidth * 6 - 1 downto dcachecfg.tagWidth * 5) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      is(U(6)) {
        tagRam_refill_data(dcachecfg.tagWidth * 7 - 1 downto dcachecfg.tagWidth * 6) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      is(U(7)) {
        tagRam_refill_data(dcachecfg.tagWidth * 8 - 1 downto dcachecfg.tagWidth * 7) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
      } // dcachecfg.wayNum == 8
      default {
        tagRam_refill_data(dcachecfg.tagWidth-1 downto 0) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
    }
  } else { // wayNum == 2
    switch(victim_idx_for_refill) { // can only be used in 2 way
      is(U(1)) {
        tagRam_refill_data(dcachecfg.tagWidth * 2 - 1 downto dcachecfg.tagWidth) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
       }
      default {
        tagRam_refill_data(dcachecfg.tagWidth-1 downto 0) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
      }
    }
  }
  val tagRam_write_pkg = TagRamPort()
  tagRam_write_pkg.addr := v_index12(mshr_chosen) // in case for latch
  tagRam_write_pkg.data := tagRam_refill_data
  // tagRam_write_pkg.mask := meta_mask
  tagRam_write_pkg.enable := has_mshr_enterRefills.orR
  tagRam.map(x => x.write(address=tagRam_write_pkg.addr, data=tagRam_write_pkg.data, 
      enable=tagRam_write_pkg.enable))
  
  // utils
  def getVIndex(vaddr: UInt): UInt = vaddr(dcachecfg.indexUpperBound downto dcachecfg.indexLowerBound)
  def getPTag(paddr: UInt): UInt = paddr(dcachecfg.tagUpperBound downto dcachecfg.tagLowerBound)
  def getPTagFromPExceptOffset(paddr_b: UInt): UInt = paddr_b(dcachecfg.tagUpperBound-dcachecfg.indexLowerBound downto dcachecfg.tagLowerBound-dcachecfg.indexLowerBound)
  def getPExceptOffset(paddr: UInt): UInt = paddr(AzureConsts.paddrWidth-1 downto dcachecfg.offsetUpperBound+1)
  def getPTagIndex(paddr: UInt): UInt = paddr(dcachecfg.tagUpperBound downto dcachecfg.indexLowerBound)
  def getBankIdFromVaddr(vaddr: UInt): UInt = vaddr(dcachecfg.zeroWidth+dcachecfg.bankIdxWidth-1 downto dcachecfg.zeroWidth)
  def getBankIdFromOffset(offset: UInt): UInt = offset(dcachecfg.bankIdxWidth-1 downto 0)
  def getBankOffset(offset: UInt): UInt = offset(dcachecfg.offsetWidth-1 downto dcachecfg.bankIdxWidth)
  def getBankAddr(index: UInt, idx_way: UInt, bank_offset: UInt): UInt = index @@ idx_way @@ bank_offset
  def getVictimIdxRand(valids_in: UInt, rand: UInt): UInt = {
    val ret = rand.resize(dcachecfg.idxWidth)
    when (!valids_in.andR) {
      for (i <- 0 until dcachecfg.wayNum) {
        when (valids_in(i) === False) {
          ret := U(i).resize(dcachecfg.idxWidth)
        }
      }
    }
    ret
  }
  def getWritebackPAddr(paddr_before: UInt, victim_tag: UInt, v_index: UInt): UInt = {
    paddr_before(AzureConsts.paddrWidth-1-dcachecfg.indexLowerBound downto dcachecfg.tagUpperBound+1-dcachecfg.indexLowerBound) @@
     victim_tag @@ v_index @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
  }
  def willBubble(i: UInt, has_mshr_refills: Vec[Bool]): Bool = {
    has_mshr_refills(i)
  }
  def checkFsmIdle(i: Int, has_mshr_loadings: Vec[Bool], has_mshr_wbs: Vec[Bool]): Bool = {
    // val ret = False
    val t1 = Bool()
    if (i < 2) {
      t1 := has_mshr_loadings(i) || has_mshr_wbs(i)
    } else {
      t1 := False
    }
    !t1
  }
}

object DCache {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new DCache)
  }
}