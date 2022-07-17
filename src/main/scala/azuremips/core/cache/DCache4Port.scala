package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class DCache4port(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val dreqs = for (i <- 0 until config.dcache.portNum) yield {
      in(new DReq())
    }
    val dresps = for (i <- 0 until config.dcache.portNum) yield {
      out(new DResp())
    }
    val cresp = in(new CResp())
    val creq = out(new CReq())
  }
  
  // some rename
  val dcachecfg = config.dcache
  val vaddr_valids = Vec(Bool(), dcachecfg.portNum)
  (vaddr_valids zip io.dreqs).foreach {case(vaddr_valid, dreq) => vaddr_valid := dreq.vaddr_valid}
  val paddrs = for (dreq <- io.dreqs) yield {
    dreq.paddr
  }
  val paddr_valids = Vec(Bool(), dcachecfg.portNum)
  (paddr_valids zip io.dreqs).foreach {case(paddr_valid, dreq) => paddr_valid := dreq.paddr_valid}
  val has_mshr_loadings = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_wbs = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_refills = Vec(Bool(), dcachecfg.portNum)
  val has_mshr_wait_cbuses = Vec(Bool(), dcachecfg.portNum)
  val v_index12 = Vec(RegInit(U(0, dcachecfg.indexWidth bits)), dcachecfg.portNum)
  val which_dreq23_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    RamPortInfo() // dreqId valid
  }
  val stall_12 = Bool() // Vec(Bool(), dcachecfg.portNum) // todo
  val stall_23 = Bool() // Vec(Bool(), dcachecfg.portNum) // todo
  stall_23 := False
  stall_12 := False
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
  val tagRam = for (i <- 0 until config.dcache.portNum) yield {
    Mem(UInt(dcachecfg.tagRamWordWidth bits), dcachecfg.setNum)
  }
  // meta, i.e. valid/dirty ram
  val validRam = Vec(RegInit(U(0, dcachecfg.validRamWordWidth bits)), dcachecfg.setNum)
  val validRam_nxt = Vec(UInt(dcachecfg.validRamWordWidth bits), dcachecfg.setNum)
  validRam_nxt := validRam
  when(!stall_12) {
    validRam := validRam_nxt
  }
  val dirtyRam = Vec(RegInit(U(0, dcachecfg.dirtyRamWordWidth bits)), dcachecfg.setNum)
  val dirtyRam_nxt = Vec(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.setNum)
  dirtyRam_nxt := dirtyRam
  when(!stall_12) {
    dirtyRam := dirtyRam_nxt
  }
  // data ram, banks yield
  val dataRam = for (i <- 0 until dcachecfg.bankNum) yield {
    Mem(UInt(dcachecfg.dataRamWordWidth bits), dcachecfg.bankSize)
  }
  
  // stage 1
  val v_indexs = Vec(UInt(dcachecfg.indexWidth bits), dcachecfg.portNum)
  (v_indexs zip io.dreqs).foreach {case (v_index, dreq) => v_index := getVIndex(dreq.vaddr)}
  val which_dreq12_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    RamPortInfo() // dreqId valid
  }
  val which_ram12_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    DReqPortInfo() // ramPortId
  }
  which_ram12_nxt.foreach {case(x) => x.ramPortId := U(0)} // in case for latch
  // shuffle
  val shuffle = new Area {
    val bank_ids = Vec(UInt(dcachecfg.bankIdxWidth bits), dcachecfg.portNum)
    val dreq_filled = Vec(Bool(), dcachecfg.portNum)
    for (i <- 0 until dcachecfg.portNum) {
      bank_ids(i) := getBankIdFromVaddr(io.dreqs(i).vaddr) // assign
      dreq_filled(i) := False // initialisation
      which_dreq12_nxt(i).valid := False // initialisation
      which_dreq12_nxt(i).dreqId := U(0) // initialisation
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank0_port0 / bank1_port0
      when (vaddr_valids(i) && bank_ids(i) === BANK0) { 
        which_dreq12_nxt(BANK0_PORT0).dreqId := U(i).resized
        which_dreq12_nxt(BANK0_PORT0).valid := True
        dreq_filled(i) := True
        which_ram12_nxt(i).ramPortId := BANK0_PORT0
      }.elsewhen (vaddr_valids(i) && bank_ids(i) === BANK1) { 
        which_dreq12_nxt(BANK1_PORT0).dreqId := U(i).resized
        dreq_filled(i) := True
        which_dreq12_nxt(BANK1_PORT0).valid := True
        which_ram12_nxt(i).ramPortId := BANK1_PORT0
      }
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank0_port1 / bank1_port1
      when (vaddr_valids(i) && !dreq_filled(i) && bank_ids(i) === BANK0) { 
        which_dreq12_nxt(BANK0_PORT1).dreqId := U(i).resized
        which_ram12_nxt(i).ramPortId := BANK0_PORT1
        which_dreq12_nxt(BANK0_PORT1).valid := True
      }.elsewhen (vaddr_valids(i) && !dreq_filled(i) && bank_ids(i) === BANK1) { 
        which_dreq12_nxt(BANK1_PORT1).dreqId := U(i).resized
        which_ram12_nxt(i).ramPortId := BANK1_PORT1
        which_dreq12_nxt(BANK1_PORT1).valid := True
      } 
    }
  } // shuffle area end
  val bubble = new Area {// refill needs a bubble for the signal repeated
    when (willBubble(BANK0_PORT0, has_mshr_refills) || willBubble(BANK0_PORT1, has_mshr_refills)) {
      which_dreq12_nxt(BANK0_PORT0).valid := False
      which_dreq12_nxt(BANK0_PORT1).valid := False
    }
    when (willBubble(BANK1_PORT0, has_mshr_refills) || willBubble(BANK1_PORT1, has_mshr_refills)) {
      which_dreq12_nxt(BANK1_PORT0).valid := False
      which_dreq12_nxt(BANK1_PORT1).valid := False
    }
  }
  // read tags 
  val tags = Vec(UInt(dcachecfg.tagRamWordWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    tags(i) := tagRam(i).readAsync(address=getVIndex(io.dreqs(i).vaddr), readUnderWrite=writeFirst)
  }
  val tags_for_match = Vec(Vec(UInt(dcachecfg.tagWidth bits), dcachecfg.wayNum), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    for (j <- 0 until dcachecfg.wayNum) {
      tags_for_match(i)(j) := tags(i)(j*dcachecfg.tagWidth+dcachecfg.tagWidth-1 downto j*dcachecfg.tagWidth)
    }
  }
  val valids = Vec(UInt(dcachecfg.validRamWordWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    valids(i) := validRam(getVIndex(io.dreqs(i).vaddr))
  }
  val dirtys = Vec(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.portNum)
  // for(i <- 0 until dcachecfg.portNum) { // read dirtys
  //   val v_index_i = getVIndex(io.dreqs(i).vaddr)
  //   dirtys(i) := dirtyRam_nxt(v_index_i)
  //   // for (j <- 0 until dcachecfg.portNum) { // dirty ram writeFirst logic
  //   //   when (which_dreq23_nxt(j).valid && v_index12(j) === v_index_i) {
  //   //     dirtys(i) := dirtyRam_nxt(v_index_i)
  //   //   }
  //   // }
  // }
  val ptags = Vec(UInt(dcachecfg.tagWidth bits), dcachecfg.portNum)
  (ptags zip io.dreqs).foreach {case (ptag, dreq) => ptag := getPTag(dreq.paddr)}
  // tag match
  val hit_bits = Vec(UInt(dcachecfg.wayNum bits), dcachecfg.portNum)
  val miss_bits = Vec(UInt(dcachecfg.wayNum bits), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    for(j <- 0 until dcachecfg.wayNum) {
      hit_bits(i)(j) := valids(i)(j) && (tags_for_match(i)(j) === ptags(i))
      miss_bits(i)(j) := !valids(i)(j) || (tags_for_match(i)(j) =/= ptags(i))
    }
  }
  val selected_idxes = hit_bits.map(x => OHToUInt(x).resize(dcachecfg.idxWidth))
  val is_hits = hit_bits.map(x => x.orR)
  // val fsm_to_hits = Vec(Bool(), dcachecfg.portNum)
  val fsm_to_hits = Vec(Bool(), dcachecfg.portNum)
  val fsm_to_misses = Vec(Bool(), dcachecfg.portNum) // actual miss
  for (i <- 0 until dcachecfg.portNum) {
    fsm_to_hits(i) := is_hits(i) && paddr_valids(i)
    fsm_to_misses(i) := miss_bits(i).andR && paddr_valids(i)
  }
  // for saving the request, we create a dreqcut class vector
  val dreq_cut_pkg = Vec(DReqCut(), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    // dreq_cut_pkg(i).dirty := dirtys(i)(selected_idxes(i)) && valids(i)(selected_idxes(i))// the selected_idx haven't been shuffled
    dreq_cut_pkg(i).paddr := io.dreqs(i).paddr
    dreq_cut_pkg(i).strobe := io.dreqs(i).strobe
    dreq_cut_pkg(i).data := io.dreqs(i).data
  }
  // regs 12
  // val v_index12 = Vec(RegInit(U(0, dcachecfg.indexWidth bits)), dcachecfg.portNum) // defined previously
  when (!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      v_index12(i) := getVIndex(io.dreqs(which_dreq12_nxt(i).dreqId).vaddr) // shuffle!
    }
  } 
  val tags_for_match12 = Vec(Vec(Reg(UInt(dcachecfg.tagWidth bits)), dcachecfg.wayNum), dcachecfg.portNum) 
  when (!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      tags_for_match12(i) := tags_for_match(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  val selected_idxes12 = Vec(RegInit(U(0, dcachecfg.idxWidth bits)), dcachecfg.portNum)
  when(!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      selected_idxes12(i) := selected_idxes(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  val valids12 = Vec(RegInit(U(0, dcachecfg.validRamWordWidth bits)), dcachecfg.portNum)
  when(!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      valids12(i) := valids(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  val dirtys12 = Vec(RegInit(U(0, dcachecfg.dirtyRamWordWidth bits)), dcachecfg.portNum)
  when(!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      dirtys12(i) := dirtys(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  val dreq_cut_pkg12 = Vec(Reg(DReqCut()), dcachecfg.portNum)
  when (!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      dreq_cut_pkg12(i) := dreq_cut_pkg(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  val fsm_to_misses12 = Vec(RegInit(False), dcachecfg.portNum)
  val fsm_to_hits12 = Vec(RegInit(False), dcachecfg.portNum)
  when (!stall_12) {
    for (i <- 0 until dcachecfg.portNum) {
      fsm_to_misses12(i) := fsm_to_misses(which_dreq12_nxt(i).dreqId) // shuffle!
      fsm_to_hits12(i) := fsm_to_hits(which_dreq12_nxt(i).dreqId) // shuffle!
    }
  }
  // shuffle info, aka port distribution info 
  val which_dreq12 = Vec(Reg(RamPortInfo()), dcachecfg.wayNum)
  val which_ram12 = Vec(Reg(DReqPortInfo()), dcachecfg.wayNum)
  when (!stall_12) {
    (which_dreq12 zip which_dreq12_nxt).foreach {case (req, req_nxt) => req := req_nxt}
    (which_ram12 zip which_ram12_nxt).foreach {case (ram, ram_nxt) => ram := ram_nxt}
  } 

  // stage 2 
  // change the shuffle valid bit
  // val which_dreq23_nxt = for(i <- 0 until dcachecfg.portNum) yield { // defined previously
  //   RamPortInfo() // dreqId valid
  // }
  val which_ram23_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    DReqPortInfo() // ramPortId
  }
  (which_dreq23_nxt zip which_dreq12).foreach {case (nxt23, dreq12) => nxt23 := dreq12}
  (which_ram23_nxt zip which_ram12).foreach {case (nxt23, dreq12) => nxt23 := dreq12}
  for (i <- 0 until dcachecfg.portNum) {
    when (!fsm_to_hits12(i) || !checkFsmIdle(i, has_mshr_loadings, has_mshr_wbs)) { // contains the ensureace that this port is not used by cresp
      which_dreq23_nxt(i).valid := False
    }
  }
  // gen victim idx
  val counter = RegInit(U(0, dcachecfg.idxWidth bits))
  counter := counter + 1
  val victim_idxes = valids12.map(x => getVictimIdxRand(x, counter)) // shuffled
  // gen dirty_nxt
  for (i <- 0 until dcachecfg.portNum) {
    when(which_dreq23_nxt(i).valid && dreq_cut_pkg12(i).strobe =/= U(0)) {
      dirtyRam_nxt(v_index12(i))(selected_idxes12(i)) := True
    }
  }
  // gen CACHE addr
  val cache_hit_addrs = Vec(UInt(dcachecfg.dataAddrWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    val bank_offset = getBankOffset(dreq_cut_pkg12(i).paddr(dcachecfg.offsetUpperBound downto dcachecfg.offsetLowerBound)) 
    cache_hit_addrs(i) := getBankAddr(v_index12(i), selected_idxes12(i), bank_offset)
  }
  // hit data output
  for(i <- 0 until dcachecfg.portNum) {
    io.dresps(i).hit := which_dreq23_nxt(which_ram12(i).ramPortId).valid
  }
  // (io.dresps zip which_dreq23_nxt).foreach {case (dresp, it) => dresp.hit := it.valid} 
  val dataRam_port_pkg = for(i <- 0 until dcachecfg.portNum) yield {
    DataRamPort()
  }
  
  // dataRam read/write data
  val data_pkg = for(i <- 0 until dcachecfg.portNum) yield {
    dataRam(i >>> 1).readWriteSync(address=dataRam_port_pkg(i).addr, data=dataRam_port_pkg(i).data,
      enable=dataRam_port_pkg(i).enable, write=dataRam_port_pkg(i).write, mask=dataRam_port_pkg(i).mask, 
      readUnderWrite=writeFirst
    )
  }

  // regs 23
  val which_dreq23 = Vec(Reg(RamPortInfo()), dcachecfg.wayNum)
  val which_ram23 = Vec(Reg(DReqPortInfo()), dcachecfg.wayNum)
  when (!stall_12) {
    (which_dreq23 zip which_dreq23_nxt).foreach {case (req, req_nxt) => req := req_nxt}
    (which_ram23 zip which_ram23_nxt).foreach {case (ram, ram_nxt) => ram := ram_nxt}
  }
  
  // stage 3
  // dataRam output shuffle, aka reorder
  for(i <- 0 until dcachecfg.portNum) {
    io.dresps(i).data := data_pkg(which_ram23(i).ramPortId)
  }

  // MSHR unit
  val cbus_using = Vec(Bool(), dcachecfg.portNum)
  val has_copy = Vec(UInt(dcachecfg.portNum bits), dcachecfg.portNum)
  val mshrs = for(i <- 0 until dcachecfg.portNum) yield {
    new Area {
      val addr = dreq_cut_pkg12(i).paddr
      val addr_before_offset = Reg(UInt(AzureConsts.paddrWidth-dcachecfg.offsetUpperBound-1 bits))
      val v_index = Reg(UInt(dcachecfg.indexWidth bits))
      val offset_ld = RegInit(U(0, dcachecfg.offsetWidth bits)) // reg, internal
      val offset = UInt(dcachecfg.offsetWidth bits) // wire, output
      val victim_idx = RegInit(U(0, dcachecfg.idxWidth bits))
      val selected_idx = RegInit(U(0, dcachecfg.idxWidth bits))
      val victim_tag = RegInit(U(0, dcachecfg.tagWidth bits))
      val dirty = Reg(Bool())
      val cache_addr_wb = getBankAddr(v_index, victim_idx, getBankOffset(offset))
      val cache_addr_ld = getBankAddr(v_index, victim_idx, getBankOffset(offset))
      val mshr_creq = CReq()
      val cbus_occupy = cbus_using(i)
      offset := U(0) // in case for latch
      mshr_creq.valid := False
      mshr_creq.is_write := False
      mshr_creq.size := CReq.MSIZE4
      mshr_creq.addr := U(0)
      mshr_creq.strobe := U(0)
      mshr_creq.data := U(0)
      mshr_creq.burst := CReq.AXI_BURST_INCR
      mshr_creq.len := CReq.MLEN16

      val tagRam_refill_data = UInt(dcachecfg.tagRamWordWidth bits)
      val validRam_refill_data = UInt(dcachecfg.validRamWordWidth bits)
      tagRam_refill_data := U(0)
      validRam_refill_data := U(0)
      val meta_mask = Bits(8 bits)
      if(dcachecfg.wayNum == 4) {
        switch(victim_idx) { // can only be used in 4 way
          is(U(1)) {
            tagRam_refill_data(dcachecfg.tagWidth * 2 - 1 downto dcachecfg.tagWidth) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(1) := True
            meta_mask := B"00001100"
          }
          is(U(2)) {
            tagRam_refill_data(dcachecfg.tagWidth * 3 - 1 downto dcachecfg.tagWidth * 2) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(2) := True
            meta_mask := B"00110000"
          }
          is(U(3)) {
            tagRam_refill_data(dcachecfg.tagWidth * 4 - 1 downto dcachecfg.tagWidth * 3) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(3) := True
            meta_mask := B"11000000"
          }
          default {
            tagRam_refill_data(dcachecfg.tagWidth-1 downto 0) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(0) := True
            meta_mask := B"00000011"
          }
        }
      } else { // wayNum == 2
        switch(victim_idx) { // can only be used in 4 way
          is(U(1)) {
            tagRam_refill_data(dcachecfg.tagWidth * 2 - 1 downto dcachecfg.tagWidth) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(1) := True
            meta_mask := B"11110000"
          }
          default {
            tagRam_refill_data(dcachecfg.tagWidth-1 downto 0) := getPTagFromPExceptOffset(addr_before_offset)
            validRam_refill_data(0) := True
            meta_mask := B"00001111"
          }
        }
      }

      val fsm_mshr = new StateMachine {
        val IDLE: State = new State with EntryPoint {
          whenIsActive {
            mshr_creq.valid := False
            when (!has_copy(i).orR && which_dreq12(i).valid && fsm_to_misses12(i)) {
              addr_before_offset := getPExceptOffset(addr)
              v_index := v_index12(i)
              offset_ld := U(0)
              victim_idx := victim_idxes(i)
              victim_tag := tags_for_match12(i)(victim_idxes(i))
              selected_idx := selected_idxes12(i)
              // dirty := dirtys12(i)(victim_idxes(i)) && valids12(i)(victim_idxes(i))
              validRam_nxt(v_index)(victim_idx) := False // write validRam
              goto(WAIT_CBUS)
            }
          }
        } // IDLE end
        val WAIT_CBUS: State = new State {
          whenIsActive {
            // 
            when (dirtyRam_nxt(v_index)(victim_idx) && cbus_occupy) {
              goto(WRITEBACK)
            }.elsewhen(cbus_occupy) {
              goto(LOAD)
            }
          }
        } // WAIT_CBUS end
        val WRITEBACK: State = new State {
          whenIsActive {
            offset := offset_ld
            mshr_creq.valid := True
            mshr_creq.is_write := True
            mshr_creq.size := CReq.MSIZE4
            mshr_creq.addr := getWritebackPAddr(addr_before_offset, victim_tag, v_index)
            mshr_creq.strobe := U"1111"
            mshr_creq.data := getWbDataFromRam(i, data_pkg, offset_ld)
            mshr_creq.burst := CReq.AXI_BURST_INCR
            mshr_creq.len := CReq.MLEN16
            when (io.cresp.ready) {
              offset := offset_ld + 1
              offset_ld := offset_ld + 1
            }
            when(io.cresp.last) {
              offset_ld := U(0)
              goto(LOAD)
            }
          }
        } // WRITEBACK end 
        val LOAD: State = new State {
          whenIsActive {
            offset := offset_ld
            mshr_creq.valid := True
            mshr_creq.is_write := False
            mshr_creq.size := CReq.MSIZE4
            mshr_creq.addr := addr_before_offset @@ U(0, dcachecfg.offsetWidth+dcachecfg.zeroWidth bits)
            mshr_creq.strobe := U"0000"
            mshr_creq.data := U(0)
            mshr_creq.burst := CReq.AXI_BURST_INCR
            mshr_creq.len := CReq.MLEN16
            when (io.cresp.ready) {
              offset_ld := offset_ld + 1
            }
            when(io.cresp.last) {
              dirtyRam_nxt(v_index)(victim_idx) := False // write dirtyRam
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
      }

    } // MSHR defination
  } // MSHRs defination
  

  // CBUS arbiter for mshrs
  val has_mshr_waitings = Vec(Bool(), dcachecfg.portNum)
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_waitings(i) := mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.WAIT_CBUS)
  }
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
  // has_mshr_wait_cbuses
  for (i <- 0 until dcachecfg.portNum) {
    has_mshr_wait_cbuses(i) := mshrs(i).fsm_mshr.isActive(mshrs(i).fsm_mshr.WAIT_CBUS)
  }

  var fsm_arbiter = new StateMachine {
    val mshr_id = RegInit(U(0, dcachecfg.portIdxWidth bits))
    val mshr_id_nxt = UInt(dcachecfg.portIdxWidth bits)
    mshr_id_nxt := U(0)
    // val mshr_chosen = UInt(dcachecfg.portIdxWidth bits) // todo
    // for(i <- 0 until dcachecfg.portNum) { when(mshr_id === U(i).resize(dcachecfg.portIdxWidth)) {
    //   mshr_chosen = U(i).resized
    // }}
    val IDLE : State = new State with EntryPoint {
      whenIsActive {
        for (i <- 0 until dcachecfg.portNum) {
          when (has_mshr_waitings(i)) {
            mshr_id_nxt := U(i).resized
          }
        }
        when (has_mshr_waitings.orR) {
          mshr_id := mshr_id_nxt
          goto(BUSY)
        }
      }
    } // IDLE end
    val BUSY : State = new State {
      whenIsActive {
        // chose one mshr
        for(i <- 0 until dcachecfg.portNum) { when(mshr_id === U(i).resize(dcachecfg.portIdxWidth)) {
          io.creq := mshrs(i).mshr_creq
        }}
        when (io.cresp.last && has_mshr_loadings(mshr_id)) {
          for (i <- 0 until dcachecfg.portNum) {
            when (has_mshr_waitings(i)) {
              mshr_id_nxt := U(i).resized
            }
          }
          when (has_mshr_waitings.orR) {
            mshr_id := mshr_id_nxt // stay in busy
          }.otherwise {
            goto(IDLE)
          }
        }
      }
    } // BUSY end
  }

  // read dirtys, stage 1
  for(i <- 0 until dcachecfg.portNum) {
    dirtys(i) := dirtyRam_nxt(getVIndex(io.dreqs(i).vaddr))
  }

  // mux before dataRam, stage 2
  cbus_using := Vec(False, dcachecfg.portNum) // in case for latch
  for(i <- 0 until dcachecfg.portNum) { // in case for latch. dataRam_port
    dataRam_port_pkg(i).addr := cache_hit_addrs(i)
    dataRam_port_pkg(i).data := dreq_cut_pkg12(i).data
    dataRam_port_pkg(i).mask := dreq_cut_pkg12(i).strobe.asBits
    dataRam_port_pkg(i).enable := which_dreq23_nxt(i).valid
  }
  val tagRam_write_pkg = TagRamPort()
  tagRam_write_pkg.addr := U(0) // in case for latch
  tagRam_write_pkg.data := U(0)
  tagRam_write_pkg.mask := B(0)
  tagRam_write_pkg.enable := False

  // find out which mshr is using cbus, update cbus_using and dataRam_port using
  for(mshr_chosen <- 0 until dcachecfg.portNum) { when(fsm_arbiter.mshr_id === U(mshr_chosen).resize(dcachecfg.portIdxWidth)) { 
    for (i <- 0 until dcachecfg.portNum) {
      cbus_using(i) := (has_mshr_waitings.orR && io.cresp.last && mshrs(mshr_chosen).fsm_mshr.isActive(mshrs(mshr_chosen).fsm_mshr.LOAD)) || 
                            (fsm_arbiter.isEntering(fsm_arbiter.BUSY) && fsm_arbiter.mshr_id_nxt === U(i))
    }
    for(i <- 0 until dcachecfg.portNum) { // the LOAD using dataRam_port
      when (!checkFsmIdle(i, has_mshr_loadings, has_mshr_wbs)) {// MSHR i and (i + 2 % 4) fsm != WB or LOAD
        dataRam_port_pkg(i).addr := Mux(mshrs(mshr_chosen).fsm_mshr.isActive(mshrs(mshr_chosen).fsm_mshr.LOAD), mshrs(mshr_chosen).cache_addr_ld, mshrs(mshr_chosen).cache_addr_wb)
        dataRam_port_pkg(i).data := io.cresp.data
        dataRam_port_pkg(i).mask := Mux(mshrs(mshr_chosen).fsm_mshr.isActive(mshrs(mshr_chosen).fsm_mshr.LOAD), B"1111", B"0000")
        dataRam_port_pkg(i).enable := Mux(getBankIdFromOffset(mshrs(mshr_chosen).offset) === getBankIdFromOffset(U(i>>>1, dcachecfg.offsetWidth bits)), True, False)
      }
    }
    tagRam_write_pkg.addr := mshrs(mshr_chosen).v_index // write tag the data
    tagRam_write_pkg.data := mshrs(mshr_chosen).tagRam_refill_data
    tagRam_write_pkg.mask := mshrs(mshr_chosen).meta_mask
    tagRam_write_pkg.enable := mshrs(mshr_chosen).fsm_mshr.isEntering(mshrs(mshr_chosen).fsm_mshr.IDLE)
  }}
  for (i <- 0 until dcachecfg.portNum) {
    for (j <- 0 until dcachecfg.portNum) {
      has_copy(i)(j) := mshrs(j).addr_before_offset === getPExceptOffset(dreq_cut_pkg12(i).paddr) && !mshrs(j).fsm_mshr.isActive(mshrs(j).fsm_mshr.IDLE)
    }
  }

  // write tag 
  tagRam.map(x => x.write(address=tagRam_write_pkg.addr, data=tagRam_write_pkg.data, 
      enable=tagRam_write_pkg.enable, mask=tagRam_write_pkg.mask))
  // validRam.map(x => x.write(address=mshr_chosen.v_index, data=mshr_chosen.validRam_refill_data, 
  //     enable=mshr_chosen.fsm_mshr.isEntering(mshr_chosen.fsm_mshr.IDLE), mask=mshr_chosen.meta_mask))
  // todo dirty ram
  // utils
  def getVIndex(vaddr: UInt): UInt = vaddr(dcachecfg.indexUpperBound downto dcachecfg.indexLowerBound)
  def getPTag(paddr: UInt): UInt = paddr(dcachecfg.tagUpperBound downto dcachecfg.tagLowerBound)
  def getPTagFromPExceptOffset(paddr_b: UInt): UInt = paddr_b(dcachecfg.tagUpperBound-dcachecfg.indexLowerBound downto dcachecfg.tagLowerBound-dcachecfg.indexLowerBound)
  def getPExceptOffset(paddr: UInt): UInt = paddr(AzureConsts.paddrWidth-1 downto dcachecfg.offsetUpperBound+1)
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
    // has_mshr_loadings(i) || has_mshr_wait_cbuses(i) || has_mshr_wbs(i)
    has_mshr_refills(i)
  }
  def checkFsmIdle(i: Int, has_mshr_loadings: Vec[Bool], has_mshr_wbs: Vec[Bool]): Bool = {
    // val ret = False
    val t1 = Bool()
    val t2 = Bool()
    if (i < 2) {
      t1 := has_mshr_loadings(i) || has_mshr_wbs(i)
      t2 := has_mshr_loadings(i+2) || has_mshr_wbs(i+2)
    } else if (i < 4) {
      t1 := has_mshr_loadings(i) || has_mshr_wbs(i)
      t2 := has_mshr_loadings(i-2) || has_mshr_wbs(i-2)
    } else {
      t1 := False
      t2 := False
    }
    !t1 && !t2
  }
  def getWbDataFromRam(i: Int, data_pkg: IndexedSeq[UInt], offset: UInt): UInt = {
    if(i < 2) {
      Mux(getBankIdFromOffset(offset) === BANK0, data_pkg(i), data_pkg(i+2))
    } else if (i < 4) {
      Mux(getBankIdFromOffset(offset) === BANK1, data_pkg(i), data_pkg(i-2))
    } else {
      data_pkg(0)
    }
  }

  def DREQ_R1 = U"00"
  def DREQ_R2 = U"01"
  def DREQ_W1 = U"10"
  def DREQ_W2 = U"11"

  def BANK0_PORT0 = U"00"
  def BANK0_PORT1 = U"01"
  def BANK1_PORT0 = U"10"
  def BANK1_PORT1 = U"11"

  def BANK0 = U"0"
  def BANK1 = U"1"
}

case class RamPortInfo(config: DCacheConfig = DCacheConfig()) extends Bundle {
  // which dreq
  val valid = Bool()
  val dreqId = UInt(config.portIdxWidth bits)
}

case class DReqPortInfo(config: DCacheConfig = DCacheConfig()) extends Bundle {
  // which ram
  val ramPortId = UInt(config.portIdxWidth bits)
}

// object DCache4port {
//   def main(args: Array[String]) {
//     // SpinalVerilog(ICache(CoreConfig()))
//     SpinalConfig(
//       defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
//     ).addStandardMemBlackboxing(blackboxAll)
//     .generateVerilog(new DCache4port)
//   }
// }