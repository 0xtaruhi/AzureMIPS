package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.cache.{DReq, DResp, CReq}
import azuremips.core.cache.{CacheInstInfo, DCacheConfig, ICacheConfig}
import azuremips.core.mmu.Mmu
import azuremips.core.ExceptionCode._
import azuremips.core.mmu.TranslateAddrReq

class CacheAccess extends Component {
  val io = new Bundle {
    val stall = in Bool()
    val mem                 = Vec(slave(new MemCachePort), 2)
    val dcache              = Vec(master(new DCachePort), 2)
    val uncache             = Vec(master(new DCachePort), 2)
    val tlbPort             = Vec(master(TranslateAddrReq()), 2)
    val dcacheInst          = out(CacheInstInfo())
    val dcacheIndexStoreTag = out UInt(DCacheConfig().tagWidth bits)
    // val icacheInst          = out(CacheInstInfo())
    // val icacheIndexStoreTag = out UInt(ICacheConfig().tagWidth bits)
    // val icacheVAddr         = out UInt(32 bits)
  }

  val mmu = for (i <- 0 until 2) yield Mmu()
  io.dcacheIndexStoreTag := 0
  val isDCacheInst = io.mem.map(_.req.isDCacheInst).reduce(_ || _)
  io.dcacheInst.isCacheInst := isDCacheInst 
  io.dcacheInst.opcode      := io.mem.map(_.req.cacheOp).reduce(_ | _)
  // io.icacheInst.isCacheInst := isICacheInst 
  // io.icacheInst.opcode      := io.mem.map(_.req.cacheOp).reduce(_ | _)
  // io.icacheVAddr            := Mux(
  //   io.mem(0).req.isICacheInst,
  //   io.mem(0).req.vaddr, io.mem(1).req.vaddr
  // )

  for (i <- 0 until 2) {
    val stage1 = new Area {
      mmu(i).io.vaddr := io.mem(i).req.vaddr
      mmu(i).io.is_write := io.mem(i).req.strobe =/= 0
      mmu(i).io.tlbPort <> io.tlbPort(i)
      val paddr   = Mux(io.dcacheInst.opcode.msb, io.mem(i).req.vaddr ,mmu(i).io.paddr)
      val uncache = mmu(i).io.uncache
      io.mem(i).exptValid := mmu(i).io.exptValid && io.mem(i).req.vaddr_valid
      io.mem(i).exptCode  := mmu(i).io.exptCode
      if (i == 1) {
        when (mmu(0).io.exptValid && io.mem(0).req.vaddr_valid) {
          io.mem(i).exptValid := True
        }
      }

      io.dcache(i).req.vaddr := io.mem(i).req.vaddr & U"32'hfffffffc"
      io.dcache(i).req.paddr := paddr & U"32'hfffffffc"
      io.dcache(i).req.size := CReq.MSIZE4
      io.dcache(i).req.strobe := io.mem(i).req.strobe
      io.dcache(i).req.data := io.mem(i).req.data
      io.uncache(i).req.vaddr := io.mem(i).req.vaddr
      io.uncache(i).req.paddr := paddr
      io.uncache(i).req.size := io.mem(i).req.size
      io.uncache(i).req.strobe := io.mem(i).req.strobe
      io.uncache(i).req.data := io.mem(i).req.data

      val reqValid = io.mem(i).req.vaddr_valid && !mmu(i).io.exptValid
      if (i == 1) {
        when (mmu(0).io.exptValid && io.mem(0).req.vaddr_valid) {
          reqValid := False
        }
      }
      io.dcache(i).req.vaddr_valid := Mux(uncache, False, io.mem(i).req.vaddr_valid)
      io.dcache(i).req.paddr_valid := Mux(uncache, False, reqValid)
      io.uncache(i).req.vaddr_valid := Mux(uncache, io.mem(i).req.vaddr_valid, False)
      io.uncache(i).req.paddr_valid := Mux(uncache, reqValid, False)
    }

    val stage2 = new Area {
      val uncache = RegNextWhen(stage1.uncache, !io.stall) init (False)
      when (uncache) {
        io.mem(i).rsp.hit := io.uncache(i).rsp.hit
      } otherwise {
        io.mem(i).rsp.hit := io.dcache(i).rsp.hit
      }
    }

    val stage3 = new Area {
      val uncache = RegNextWhen(stage2.uncache, !io.stall) init (False)
      when (uncache) {
        io.mem(i).rsp.data := io.uncache(i).rsp.data
      } otherwise {
        io.mem(i).rsp.data := io.dcache(i).rsp.data
      }
    }

  }  

  // def isUncacheAddr(addr: UInt) : Bool = {
  //   addr(31 downto 29) === U"101"
  //   // True
  // }
}

object GenCacheAccess extends App {
  SpinalVerilog(new CacheAccess)
}