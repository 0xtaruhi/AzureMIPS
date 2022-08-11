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
  val stall_12 = Bool()
  val dreq_cut_pkg12 = Vec(Reg(DReqCut()), dcachecfg.portNum)
  val has_copy = Vec(Bool(), dcachecfg.portNum) // stage 2 signal 
  val share_v_index = Vec(Bool(), dcachecfg.portNum) // stage 2 signal
  val mshr_chosen = RegInit(U(0, dcachecfg.portIdxWidth bits)) // mshr signal
  val meta_refresh_en = True
  stall_12 := False
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
  val miss_merge = Bool()
  for (i <- 0 until dcachecfg.portNum) {
    fsm_to_hits(i) := is_hits(i) && paddr_valids(i)
    fsm_to_misses(i) := ~is_hits(i) && paddr_valids(i)
  }
  miss_merge := getPTagIndex(io.dreqs(0).paddr) === getPTagIndex(io.dreqs(1).paddr)
  // for saving the request, we create a dreqcut class vector
  val dreq_cut_pkg = Vec(DReqCut(), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    dreq_cut_pkg(i).paddr := io.dreqs(i).paddr
    dreq_cut_pkg(i).strobe := io.dreqs(i).strobe
    dreq_cut_pkg(i).data := io.dreqs(i).data
  }
  
  // regs 12
  val v_index12 = Vec(RegInit(U(0, dcachecfg.indexWidth bits)), dcachecfg.portNum) 
  when (!stall_12) {
    v_index12 := v_indexs
  }
  val tags_for_match12 = Vec(Vec(Reg(UInt(dcachecfg.tagWidth bits)), dcachecfg.wayNum), dcachecfg.portNum) 
  when (!stall_12) {
    tags_for_match12 := tags_for_match
  } 
  val selected_idxes12 = Vec(RegInit(U(0, dcachecfg.idxWidth bits)), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12) {
      selected_idxes12(i) := selected_idxes(i)
    } 
  }
  val fsm_to_misses12 = Vec(RegInit(False), dcachecfg.portNum)
  val fsm_to_hits12 = Vec(RegInit(False), dcachecfg.portNum)
  val miss_merge12 = RegInit(False)
  when (!stall_12) {
    fsm_to_misses12 := fsm_to_misses
    fsm_to_hits12 := fsm_to_hits
    miss_merge12 := miss_merge
  } 
  // val dreq_cut_pkg12 = Vec(Reg(DReqCut()), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    when (!stall_12) {
      dreq_cut_pkg12(i) := dreq_cut_pkg(i)
    } 
  }

  // stage 2 
  // gen victim idx
  val counter = RegInit(U(0, dcachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idxes = Vec(UInt(dcachecfg.idxWidth bits), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    victim_idxes(i) := counter
    val valids12 = UInt(dcachecfg.validRamWordWidth bits)
    valids12 := validRam(v_index12(i))
    when (!valids12.andR) {
      for (j <- 0 until dcachecfg.wayNum) {
        when (valids12(j) === False) {
          victim_idxes(i) := U(j).resize(dcachecfg.idxWidth)
        }
      }
    }.elsewhen(fsm_to_hits12(1-i) && counter === selected_idxes12(1-i)) {
      victim_idxes(i) := ~selected_idxes12(1-i) // make them not equal 
    }
  }
  val victim_idxes12 = Vec(RegInit(U(0, dcachecfg.idxWidth bits)), dcachecfg.portNum) // this reg'll be modified in mshr_fsm

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
    io.dresps(i).hit := fsm_to_hits12(i) || isMissDataReady(i, mshr_chosen, fsm_to_misses12, has_mshr_refills, miss_merge12)
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

  // miss fsm unit
  // val mshr_chosen = RegInit(U(0, dcachecfg.portIdxWidth bits))
  val offset_ld = RegInit(U(0, dcachecfg.offsetWidth bits)) // reg, source of offset
  val offset = U(0, dcachecfg.offsetWidth bits) // wire, output
  val cache_miss_addrs = Vec(UInt(dcachecfg.dataAddrWidth bits), dcachecfg.portNum)
  val victim_tags = Vec(UInt(dcachecfg.tagWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    cache_miss_addrs(i) := v_index12(i) @@ victim_idxes12(i) @@ getBankOffset(offset)
    victim_tags(i) := tags_for_match12(i)(victim_idxes12(i))
  }
  val fsm_mshr = new StateMachine {
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        io.creqs(PORT0).valid := False
        io.creqs(PORT1).valid := False
        when (fsm_to_misses12(PORT0)) {
          offset_ld := U(0)
          victim_idxes12(PORT0) := victim_idxes(PORT0)
          victim_idxes12(PORT1) := victim_idxes(PORT0)
          mshr_chosen := PORT0 // means port 0 is using cbus
          // no need to change valid ram
          when (dirtyRam(v_index12(PORT0))(victim_idxes(PORT0))) { // read dirtyRam, use v_index12 && victim_idx, not 12
            goto(WRITEBACK0)
          }.otherwise {
            goto(LOAD0)
          }
        }.elsewhen (fsm_to_misses12(PORT1)) { // miss merge will be used later
          offset_ld := U(0)
          victim_idxes12(PORT1) := victim_idxes(PORT1)
          mshr_chosen := PORT1
          when (dirtyRam(v_index12(PORT1))(victim_idxes(PORT1))) { // read dirtyRam, use v_index12 && victim_idx, not 12
            goto(WRITEBACK1)
          }.otherwise {
            goto(LOAD1)
          }
        }
        when (fsm_to_misses12.orR) {
          stall_12 := True // equals to: isExiting IDLE
        }
      }
    } // IDLE end
    val WRITEBACK0: State = new State {
      whenIsActive {
        offset := offset_ld
        stall_12 := True

        io.creqs(PORT0).valid := True
        io.creqs(PORT0).is_write := True
        io.creqs(PORT0).size := CReq.MSIZE4
        io.creqs(PORT0).addr := getWritebackPAddr(dreq_cut_pkg12(PORT0).paddr, victim_tags(PORT0), v_index12(PORT0))
        io.creqs(PORT0).strobe := U"1111"
        io.creqs(PORT0).data := data_pkg(PORT0)
        io.creqs(PORT0).burst := CReq.AXI_BURST_INCR
        io.creqs(PORT0).len := CReq.MLEN16
        when (io.cresps(PORT0).ready) {
          offset := offset_ld + 1
          offset_ld := offset_ld + 1
        }
        when(io.cresps(PORT0).last) {
          offset_ld := U(0)
          goto(LOAD0)
        }
      }
    } // WRITEBACK0 end 
    val LOAD0: State = new State {
      whenIsActive {
        offset := offset_ld
        stall_12 := True

        io.creqs(PORT0).valid := True
        io.creqs(PORT0).is_write := False
        io.creqs(PORT0).size := CReq.MSIZE4
        io.creqs(PORT0).addr := getPExceptOffset(dreq_cut_pkg12(PORT0).paddr) @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
        io.creqs(PORT0).strobe := U"0000"
        io.creqs(PORT0).data := U(0, 32 bits)
        io.creqs(PORT0).burst := CReq.AXI_BURST_INCR
        io.creqs(PORT0).len := CReq.MLEN16
        when (io.cresps(PORT0).ready) {
          offset_ld := offset_ld + 1
        }
        when(io.cresps(PORT0).last) {
          dirtyRam_nxt(v_index12(PORT0))(victim_idxes12(PORT0)) := dreq_cut_pkg12(PORT0).strobe =/= U(0) // write dirtyRam
          validRam_nxt(v_index12(PORT0))(victim_idxes12(PORT0)) := True // write validRam
          goto(REFILL0)
        }
      }
    } // LOAD0 end 
    val REFILL0: State = new State { // for waiting tag ram fresh
      whenIsActive {

        when (!miss_merge12 && fsm_to_misses12(PORT1)) {
          stall_12 := True
          offset_ld := U(0)
          victim_idxes12(PORT1) := victim_idxes(PORT1)
          mshr_chosen := PORT1
          when (dirtyRam(v_index12(PORT1))(victim_idxes(PORT1))) { // read dirtyRam, use v_index12 && victim_idx, not 12
            goto(WRITEBACK1)
          }.otherwise {
            goto(LOAD1)
          }
        }.otherwise {
          goto(IDLE)
        }
      }
    } // REFILL0 end
    val WRITEBACK1: State = new State {
      whenIsActive {
        offset := offset_ld
        stall_12 := True

        io.creqs(PORT1).valid := True
        io.creqs(PORT1).is_write := True
        io.creqs(PORT1).size := CReq.MSIZE4
        io.creqs(PORT1).addr := getWritebackPAddr(dreq_cut_pkg12(PORT1).paddr, victim_tags(PORT1), v_index12(PORT1))
        io.creqs(PORT1).strobe := U"1111"
        io.creqs(PORT1).data := data_pkg(PORT1)
        io.creqs(PORT1).burst := CReq.AXI_BURST_INCR
        io.creqs(PORT1).len := CReq.MLEN16
        when (io.cresps(PORT1).ready) {
          offset := offset_ld + 1
          offset_ld := offset_ld + 1
        }
        when(io.cresps(PORT1).last) {
          offset_ld := U(0)
          goto(LOAD1)
        }
      }
    } // WRITEBACK1 end 
    val LOAD1: State = new State {
      whenIsActive {
        offset := offset_ld
        stall_12 := True

        io.creqs(PORT1).valid := True
        io.creqs(PORT1).is_write := False
        io.creqs(PORT1).size := CReq.MSIZE4
        io.creqs(PORT1).addr := getPExceptOffset(dreq_cut_pkg12(PORT1).paddr) @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
        io.creqs(PORT1).strobe := U"0000"
        io.creqs(PORT1).data := U(0, 32 bits)
        io.creqs(PORT1).burst := CReq.AXI_BURST_INCR
        io.creqs(PORT1).len := CReq.MLEN16
        when (io.cresps(PORT1).ready) {
          offset_ld := offset_ld + 1
        }
        when(io.cresps(PORT1).last) {
          dirtyRam_nxt(v_index12(PORT1))(victim_idxes12(PORT1)) := dreq_cut_pkg12(PORT1).strobe =/= U(0) // write dirtyRam
          validRam_nxt(v_index12(PORT1))(victim_idxes12(PORT1)) := True // write validRam
          goto(REFILL1)
        }
      }
    } // LOAD1 end 
    val REFILL1: State = new State { // for waiting tag ram fresh
      whenIsActive {
        mshr_chosen := PORT0
        goto(IDLE)
      }
    } // REFILL1 end
  } // fsm defination  

  // read state for mshrs.fsm_mshr
  has_mshr_loadings(0) := fsm_mshr.isActive(fsm_mshr.LOAD0)
  has_mshr_loadings(1) := fsm_mshr.isActive(fsm_mshr.LOAD1)
  has_mshr_wbs(0) := fsm_mshr.isActive(fsm_mshr.WRITEBACK0)
  has_mshr_wbs(1) := fsm_mshr.isActive(fsm_mshr.WRITEBACK1)
  has_mshr_refills(0) := fsm_mshr.isActive(fsm_mshr.REFILL0)
  has_mshr_refills(1) := fsm_mshr.isActive(fsm_mshr.REFILL1)

  // mux before dataRam, stage 2
  // data port
  for(i <- 0 until dcachecfg.portNum) { // the LOAD using dataRam_port
    when (!checkFsmIdle(i, has_mshr_loadings, has_mshr_wbs)) {// MSHR i fsm == WB or LOAD
      dataRam_port_pkg(i).addr := cache_miss_addrs(i)
      dataRam_port_pkg(i).data := io.cresps(i).data
      dataRam_port_pkg(i).mask := Mux(has_mshr_loadings(i), B"1111", B"0000")
      dataRam_port_pkg(i).enable := True
    }.elsewhen (isMissDataReady(i, mshr_chosen, fsm_to_misses12, has_mshr_refills, miss_merge12)) { // this or other port, use fsm_to_miss12. 
      dataRam_port_pkg(i).addr := v_index12(i) @@ victim_idxes12(i) @@ 
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
  // write tag 
  val tagRam_refill_data = UInt(dcachecfg.tagRamWordWidth bits)
  
  val victim_idx_for_refill = victim_idxes12(mshr_chosen)
  tagRam_refill_data := (tags_for_match12(mshr_chosen).asBits).asUInt
  switch(victim_idx_for_refill) { // can only be used in 4 way
    is(U(1)) {
      tagRam_refill_data(dcachecfg.tagWidth * 2 - 1 downto dcachecfg.tagWidth) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
    }
    if(dcachecfg.wayNum >= 4) {
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
    } // dcachecfg.wayNum == 4
    default {
      tagRam_refill_data(dcachecfg.tagWidth-1 downto 0) := getPTag(dreq_cut_pkg12(mshr_chosen).paddr)
    }
  }
  val tagRam_write_pkg = TagRamPort()
  tagRam_write_pkg.addr := v_index12(mshr_chosen) // in case for latch
  tagRam_write_pkg.data := tagRam_refill_data
  tagRam_write_pkg.enable := (fsm_mshr.isActive(fsm_mshr.LOAD0) || fsm_mshr.isActive(fsm_mshr.LOAD1)) && (io.cresps(0).last || io.cresps(1).last)
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
  def isMissDataReady(i: Int, mshr_chosen: UInt, fsm_to_misses12: Vec[Bool], has_mshr_refills: Vec[Bool], miss_merge12: Bool): Bool = {
    if(i == 0) {
      (mshr_chosen === PORT1 && fsm_to_misses12.andR) || has_mshr_refills(PORT0)
    } else {
      (has_mshr_refills(PORT0) && miss_merge12 && fsm_to_misses12(PORT1)) || has_mshr_refills(PORT1)
    }
  }
  def getWritebackPAddr(paddr: UInt, victim_tag: UInt, v_index: UInt): UInt = {
    paddr(32-1 downto dcachecfg.tagUpperBound+1) @@
     victim_tag @@ v_index @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
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

  def PORT0 = U"0"
  def PORT1 = U"1"
}

object DCache {
  def main(args: Array[String]) {
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new DCache)
  }
}