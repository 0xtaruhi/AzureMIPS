// package azuremips.core.lsu

// import spinal.core._
// import spinal.lib._
// import azuremips.core._
// import azuremips.core.Uops._
// import azuremips.core.exu.ExecutedSignals

// class CommittedSignals extends Bundle {
//   val wrRegEn   = Bool()
//   val wrData    = UInt(32 bits)
//   val wrRegAddr = UInt(6 bits)
//   val exptEn    = Bool()
//   val exptCode  = UInt()
// }

// class DCachePort extends Bundle with IMasterSlave {
//   val req = new DReq()
//   val rsp = new DRsp()

//   override def asMaster(): Unit = {
//     out(req)
//     in(rsp)
//   }
// }

// class Mem extends Component {
//   val io = new Bundle {
//     val excutedSignals = in Vec(new ExecutedSignals, 2)
//     val dcache = Vec(master(new DCachePort), 2)
//     val uncache = Vec(master(new DCachePort), 2)
//     val prevStall = out Bool()
//   }

//   // (io.excutedSignals zip (io.dcache zip io.uncache)).foreach{
//   //   case (execute, (dcache, uncache)) => {
//   //     dcache.req.vaddr  := execute.memVAddr
//   //     uncache.req.vaddr := execute.memVAddr
//   //     val stage1 = new Area {
//   //       val stall     = False
//   //       val isUncache = False
//   //       when (execute.memVAddr(31 downto 29) === U"101") {
//   //         isUncache := True
//   //       }
//   //       dcache.req.vaddr_valid  := False
//   //       uncache.req.vaddr_valid := False
//   //       dcache.req.paddr_valid  := False
//   //       uncache.req.paddr_valid := False
//   //       when (!stall) {
//   //         dcache.req.vaddr_valid  := Mux(isUncache, False, True)
//   //         uncache.req.vaddr_valid := Mux(isUncache, True, False)
//   //         dcache.req.paddr_valid  := Mux(isUncache, False, True)
//   //         uncache.req.paddr_valid := Mux(isUncache, True, False)
//   //       }
//   //       val paddr = UInt(32 bits)
//   //       when (execute.memVAddr(31 downto 30) === U"10") {
//   //         paddr := U"32'h0fff" & execute.memVaddr
//   //       }
//   //       dcache.req.paddr  := paddr
//   //       uncache.req.paddr := paddr
//   //     }
//   //     val stage2 = new Area {
//   //       val wrMemEn = RegNext(execute.wrMemEn)
//   //       val rdMemEn = RegNext(execute.rdMemEn)
//   //       val isUncache = RegNext(stage1.isUncache)
//   //       val stall   = False
//   //       when (rdMemEn && (isUncache || !isUncache && !dcache.rsp.hit) {
//   //         stall := True ; stage1.stall := True
//   //       }
//   //     }
//   //   }
//   // }  

//   val stage1 = new Area {
//     val stall     = False
//     val isUncache = Vec(False, 2)
//     for (i <- 0 until 2) {
//       when (io.excutedSignals(i).memVAddr(31 downto 29) === U"101") {
//         isUncache(i) := True
//       }
//       io.dcache(i).req.vaddr_valid  := False
//       io.uncache(i).req.vaddr_valid := False
//       io.dcache(i).req.paddr_valid  := False
//       io.uncache(i).req.paddr_valid := False
//       when (!stall) {
//         io.dcache(i).req.vaddr_valid  := Mux(isUncache(i), False, True)
//         io.uncache(i).req.vaddr_valid := Mux(isUncache(i), True, False)
//         io.dcache(i).req.paddr_valid  := Mux(isUncache(i), False, True)
//         io.uncache(i).req.paddr_valid := Mux(isUncache(i), True, False)
//       }
//       val paddr = UInt(32 bits)
//       when (io.excutedSignals(i).memVAddr(31 downto 30) === U"10") {
//         paddr := U"32'h0fff" & io.excutedSignals(i).memVaddr
//       }
//       io.dcache(i).req.paddr  := paddr
//       io.uncache(i).req.paddr := paddr
//     }
//   }

//   val stage2 = new Area {
//     val stall = False
//     val isUnchae = RegNextWhen(stage1.isUncache, !stall)
//     stall := for (i <- 0 until 2) yield {
//       io.excutedSignals(i).wrMemEn && (isUnchae(i) || !isUnchae(i) && !io.dcache(i).rsp.hit)
//     }.reduce(_ || _)
//     stage1.stall := stall
//   }

//   val stage3 = new Area {

//   }
// }