package azuremips.core.cache

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import azuremips.core._

case class DCache(config: CoreConfig = CoreConfig()) extends Component {
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
  val stall_12 = Bool() // Vec(Bool(), dcachecfg.portNum) // todo
  val stall_23 = Bool() // Vec(Bool(), dcachecfg.portNum) // todo
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
  val validRam = for (i <- 0 until config.dcache.portNum) yield {
    Mem(UInt(dcachecfg.validRamWordWidth bits), dcachecfg.setNum)
  }
  val dirtyRam = for (i <- 0 until config.dcache.portNum) yield {
    Mem(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.setNum)
  }
  // data ram, banks yield
  val dataRam = for (i <- 0 until dcachecfg.bankNum) yield {
    Mem(UInt(dcachecfg.dataRamWordWidth bits), dcachecfg.bankSize)
  }
  
  // stage 1
  val v_indexs = Vec(UInt(dcachecfg.indexWidth bits), dcachecfg.portNum)
  (v_indexs zip io.dreqs).foreach {case (v_index, dreq) => v_index := dreq.vaddr }
  val which_dreq_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    RamPortInfo() // dreqId valid
  }
  val which_ram_nxt = for(i <- 0 until dcachecfg.portNum) yield {
    DReqPortInfo() // ramPortId
  }
  // shuffle
  val shuffle = new Area {
    val bank_ids = Vec(UInt(dcachecfg.bankIdxWidth bits), dcachecfg.portNum)
    val dreq_filled = Vec(Bool(), dcachecfg.portNum)
    for (i <- 0 until dcachecfg.portNum) {
      bank_ids(i) := getBankIdFromVaddr(io.dreqs(i).vaddr) // assign
      dreq_filled(i) := False // initialisation
      which_dreq_nxt(i).valid := False // initialisation
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank0_port0
      when (vaddr_valids(i) && bank_ids(i) === BANK0) { 
        which_dreq_nxt(BANK0_PORT0).dreqId := U(i).resized
        which_dreq_nxt(i).valid := True
        dreq_filled(i) := True
        which_ram_nxt(i).ramPortId := BANK0_PORT0
      }
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank1_port0
      when (vaddr_valids(i) && bank_ids(i) === BANK1) { 
        which_dreq_nxt(BANK1_PORT0).dreqId := U(i).resized
        dreq_filled(i) := True
        which_dreq_nxt(i).valid := True
        which_ram_nxt(i).ramPortId := BANK1_PORT0
      }
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank0_port1
      when (vaddr_valids(i) && !dreq_filled(i) && bank_ids(i) === BANK0) { 
        which_dreq_nxt(BANK0_PORT1).dreqId := U(i).resized
        which_ram_nxt(i).ramPortId := BANK0_PORT1
        which_dreq_nxt(i).valid := True
      }
    }
    for (i <- dcachecfg.portNum-1 to 0 by -1) {// go through dreq for bank1_port1
      when (vaddr_valids(i) && !dreq_filled(i) && bank_ids(i) === BANK1) { 
        which_dreq_nxt(BANK1_PORT1).dreqId := U(i).resized
        which_ram_nxt(i).ramPortId := BANK1_PORT1
        which_dreq_nxt(i).valid := True
      }
    }
  } // shuffle area end
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
    valids(i) := validRam(i).readAsync(address=getVIndex(io.dreqs(i).vaddr), readUnderWrite=writeFirst)
  }
  val dirtys = Vec(UInt(dcachecfg.dirtyRamWordWidth bits), dcachecfg.portNum)
  for(i <- 0 until dcachecfg.portNum) {
    dirtys(i) := dirtyRam(i).readAsync(address=getVIndex(io.dreqs(i).vaddr), readUnderWrite=writeFirst)
  }
  val ptags = Vec(U(0, dcachecfg.tagWidth bits), dcachecfg.portNum)
  (ptags zip io.dreqs).foreach {case (ptag, dreq) => ptag := getPTag(dreq.paddr)}
  // tag match
  val hit_bits = Vec(UInt(dcachecfg.portNum))
  
  // regs 12
  // val tags12 = Vec(Vec(RegInit(U(0, dcachecfg.tagWidth bits)), dcachecfg.wayNum), dcachecfg.portNum)
  // when (!stall_12) {
  //   for(i <- 0 until dcachecfg.portNum) {
  //     for (j <- 0 until dcachecfg.wayNum) {
  //       tags12(i)(j) := tags(i)(j*dcachecfg.tagWidth+dcachecfg.tagWidth-1 downto j*dcachecfg.tagWidth)
  //     }
  //   }
  // }
  // val valids12 = Vec(RegInit(U(0, dcachecfg.validRamWordWidth bits)), dcachecfg.portNum)
  // val dirtys12 = Vec(RegInit(U(0, dcachecfg.dirtyRamWordWidth bits)), dcachecfg.portNum)
  // when (!stall_12) {
  //   valids12 := valids
  //   dirtys12 := dirtys
  // }
  val v_index12 = Vec(RegInit(U(0, dcachecfg.indexWidth bits)), dcachecfg.portNum)
  // when (!stall_12) {
  //   for (i <- 0 until dcachecfg.portNum) {
  //     v_index12(i) := getVIndex(io.dreqs(which_dreq_nxt(i).dreqId).vaddr) // shuffle!
  //   }
  // } 
  // shuffle info / port distribution 
  val which_dreq = Vec(Reg(RamPortInfo()), dcachecfg.wayNum)
  val which_ram = Vec(Reg(DReqPortInfo()), dcachecfg.wayNum)
  when (!stall_12) {
    (which_dreq zip which_dreq_nxt).foreach {case (req, req_nxt) => req := req_nxt}
    (which_ram zip which_ram_nxt).foreach {case (ram, ram_nxt) => ram := ram_nxt}
  } 

  // stage 2 
  
  // utils
  def getVIndex(vaddr: UInt): UInt = vaddr(dcachecfg.indexUpperBound downto dcachecfg.indexLowerBound)
  def getPTag(paddr: UInt): UInt = paddr(dcachecfg.tagUpperBound downto tagLowerBound)
  def getBankIdFromVaddr(vaddr: UInt): UInt = vaddr(dcachecfg.zeroWidth+dcachecfg.bankIdxWidth-1 downto dcachecfg.zeroWidth)
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
  val valid = Bool()
  val dreqId = UInt(config.portIdxWidth bits)
}

case class DReqPortInfo(config: DCacheConfig = DCacheConfig()) extends Bundle {
  val ramPortId = UInt(config.portIdxWidth bits)
}

object DCache {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new DCache)
  }
}