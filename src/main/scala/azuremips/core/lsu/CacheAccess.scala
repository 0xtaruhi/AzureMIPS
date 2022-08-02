package azuremips.core.lsu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.cache.{DReq, DResp, CReq}

class CacheAccess extends Component {
  val io = new Bundle {
    val stall = in Bool()
    val mem = Vec(slave(new DCachePort), 2)
    val dcache = Vec(master(new DCachePort), 2)
    val uncache = Vec(master(new DCachePort), 2)
  }

  for (i <- 0 until 2) {
    val stage1 = new Area {
      val uncache = isUncacheAddr(io.mem(i).req.vaddr)

      io.dcache(i).req.vaddr := io.mem(i).req.vaddr & U"32'hfffffffc"
      io.dcache(i).req.paddr := io.mem(i).req.paddr & U"32'hfffffffc"
      io.dcache(i).req.size := CReq.MSIZE4
      io.dcache(i).req.strobe := io.mem(i).req.strobe
      io.dcache(i).req.data := io.mem(i).req.data
      io.uncache(i).req.vaddr := io.mem(i).req.vaddr
      io.uncache(i).req.paddr := io.mem(i).req.paddr
      io.uncache(i).req.size := io.mem(i).req.size
      io.uncache(i).req.strobe := io.mem(i).req.strobe
      io.uncache(i).req.data := io.mem(i).req.data

      io.dcache(i).req.vaddr_valid := Mux(uncache, False, io.mem(i).req.vaddr_valid)
      io.dcache(i).req.paddr_valid := Mux(uncache, False, io.mem(i).req.paddr_valid)
      io.uncache(i).req.vaddr_valid := Mux(uncache, io.mem(i).req.vaddr_valid, False)
      io.uncache(i).req.paddr_valid := Mux(uncache, io.mem(i).req.paddr_valid, False)
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

  def isUncacheAddr(addr: UInt) : Bool = {
    addr(31 downto 29) === U"101"
    // True
  }
}

object GenCacheAccess extends App {
  SpinalVerilog(new CacheAccess)
}