package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core._

case class CReq() extends Bundle {
    import AzureConsts._
    import Mips._
    val valid = Bool() // in request?
    val is_write = Bool()  // is it a write transaction?
    val size = UInt(3 bits)     // number of bytes in one burst
    val addr = UInt(paddrWidth bits)      // start address
    val strobe = UInt(4 bits)   // which bytes are enabled?
    val data = UInt(wordWidth bits)      // the data to write
    val burst = UInt(2 bits)           // the burst type
    val len = UInt(4 bits)       // number of bursts
}

object CReq {
    def MSIZE1 = U"000"
    def MSIZE2 = U"001"
    def MSIZE4 = U"010"

    def MLEN1  = U"0000"
    def MLEN2  = U"0001"
    def MLEN4  = U"0011"
    def MLEN8  = U"0111"
    def MLEN16 = U"1111"

    def AXI_BURST_FIXED = U"00"
    def AXI_BURST_INCR = U"01"
    def AXI_BURST_WRAP = U"10"
    def AXI_BURST_RESERVED = U"11"
}

case class CResp() extends Bundle {
    import Mips._
    val ready = Bool()
    val last = Bool()
    
    val data = UInt(wordWidth bits) 
}

case class DReq() extends Bundle {
    import Mips._
    val vaddr_valid = Bool()
    val vaddr = UInt(32 bits)
    val paddr_valid = Bool()
    val paddr = UInt(32 bits)
    val strobe = UInt(4 bits)
    val size = UInt(3 bits)    // number of bytes. in cached dreq, it's ignored

    val data = UInt(wordWidth bits) 
}

object DReq {
    def MSIZE1 = U"000"
    def MSIZE2 = U"001"
    def MSIZE4 = U"010"
}

case class DResp() extends Bundle {
    import Mips._
    val hit = Bool()

    val data = UInt(wordWidth bits)
}

case class DReqCut(config: DCacheConfig = DCacheConfig()) extends Bundle {
    import Mips._
    val paddr = UInt(AzureConsts.paddrWidth bits)
    val strobe = UInt(4 bits)
    val data = UInt(wordWidth bits)
    // val dirtys = Vec(Bool(). config.portNum)
}

case class DataRamPort(config: DCacheConfig = DCacheConfig()) extends Bundle {
    val addr = UInt(config.dataAddrWidth bits)
    val mask = Bits(4 bits)
    val data = UInt(Mips.wordWidth bits)
    val enable = Bool()

    val write = (mask =/= B(0))
}

case class TagRamPort(config: DCacheConfig = DCacheConfig()) extends Bundle {
    val addr = UInt(config.indexWidth bits)
    val data = UInt(config.tagRamWordWidth bits)
    val mask = Bits(8 bits)
    val enable = Bool()
}
