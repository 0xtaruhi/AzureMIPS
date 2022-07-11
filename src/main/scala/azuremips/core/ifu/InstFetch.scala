package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.utils._

case class JumpInfo(config: CoreConfig) extends Bundle {
  val jmpEn = Bool()
  val jmpDest = UInt(32 bits)
}

case class IF2ICache(config: CoreConfig) extends Bundle with IMasterSlave {
  import AzureConsts._
  val vaddr = UInt(vaddrWidth bits)
  val vaddr_valid = Bool()
  val paddr = UInt(paddrWidth bits)
  val paddr_valid = Bool()
  val instValids = Vec(Bool(), config.icache.bankNum)
  val insts = Vec(UInt(32 bits), config.icache.bankNum)
  val hit = Bool()

  override def asMaster() {
    in (instValids, insts, hit)
    out (vaddr, vaddr_valid, paddr, paddr_valid)
  }
}

// class InstFetch(
//   val config: CoreConfig = CoreConfig()
// ) extends Component {
//   val io = new Bundle {
//     val ifJmp        = in(JumpInfo(config))
//     val fetchBufFull = in Bool()
//     val icache       = master(IF2ICache(config))
//     // val instsPack    = out Vec(Flow(UInt(32 bits)), config.ifConfig.instFetchNum)
//   }

//   def instFetchNum = config.ifConfig.instFetchNum

//   // IF0
//   val if2JumpInfo = JumpInfo(config)
//   val tlb = new TLB(pipeline=true, config)

//   val if0 = new Area {
//     val pc = Reg(UInt(32 bits)) init(0)
//     when (io.fetchBufFull) {
//       pc := pc
//     } elsewhen (io.ifJmp.jmpEn) {
//       pc := io.ifJmp.jmpDest
//     } elsewhen (if2JumpInfo.jmpEn) {
//       pc := if2JumpInfo.jmpDest
//     } otherwise { pc := pc + 4 * instFetchNum }  
//     tlb.io.vaddr := pc
//     io.icache.vaddr := pc
//   }

// //   io.icache.paddr := tlb.io.paddr

// //   val if1 = new Area {
// //     val pc = RegNext(if0.pc)
// //     // io.icache.vaddr := pc
// //   }

//   val if2 = new Area {
//     val pc = RegNext(if1.pc)
//     val fastDecodes = for (i <- 0 until instFetchNum) yield {
//       val fastDecode = new FastDecode
//       fastDecode.io.inst := io.icache.insts(i)
//       fastDecode
//     }
//     val hasBr = fastDecodes.map(_.io.isBr).reduce(_ || _)
//     val fastDecodesIdxWidth = log2Up(instFetchNum)
//     val brInstIdx = PriorityMux(for (i <- 0 until instFetchNum) yield {
//       (fastDecodes(i).io.isBr, U(i, fastDecodesIdxWidth bits))
//     })
//     val lastInstIsBr = (brInstIdx === U(instFetchNum - 1)) && hasBr
//     val brMask = brInstIdx.muxList(
//       for (i <- 0 until instFetchNum) yield {
//         (i, fastDecodes(i).io.brMask)
//       }
//     )
//     // Valid Inst
//     val instValidMask = UInt(instFetchNum bits)
//     when (lastInstIsBr) {
//       instValidMask := ~U(1, instFetchNum bits)
//     } otherwise {
//       for (i <- 0 until instFetchNum) {
//         instValidMask(i) := Mux(i <= brInstIdx + 1, True, False)
//       }
//     }

//     // redirect
//     if2JumpInfo.jmpEn := False
//     if2JumpInfo.jmpDest := 0

//     when (hasBr) {
//       when (lastInstIsBr) {
//         if2JumpInfo.jmpEn := True
//         if2JumpInfo.jmpDest := pc + 4 * (instFetchNum - 1)
//       } otherwise {
//         switch (brMask) {
//           import BrMaskConsts._
//           is (BRMASK_J, BRMASK_JR) {
//             if2JumpInfo.jmpEn := True
//             if2JumpInfo.jmpDest := 0
//           }
//           is (BRMASK_BX) {
//             if2JumpInfo.jmpEn := False
//             if2JumpInfo.jmpDest := 0
//           }
//         }
//       }
//     }
//   }

// }

// // object InstFetch {
// //   def main(args: Array[String]) {
// //     SpinalVerilog(new InstFetch)
// //   }
// // }