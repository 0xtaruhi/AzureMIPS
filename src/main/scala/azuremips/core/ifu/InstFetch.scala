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
//     val instsPack    = out Vec(Flow(UInt(32 bits), config.ifConfig.instFetchNum))
//   }

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
//     } otherwise { pc := pc + 4 * config.ifConfig.instFetchNum }  
//   tlb.io.vaddr := pc
//   io.icache.vaddr := pc
//   }

//   io.icache.paddr := tlb.io.paddr

//   val if1 = new Area {
//     val pc = RegNext(if0.pc)
//     // io.icache.vaddr := pc
//   }

//   val if2 = new Area {
//     val pc = RegNext(if1.pc)
//     def opcodeJ   = U"000010"
//     def opcodeJal = U"000011"
//     def isJorJal(inst: UInt): Bool = {
//       val opcode = inst(31 downto 26)
//       opcode === opcodeJ || opcode === opcodeJal
//     }
//     val hasDirectJmp = io.icache.hasBr && isJorJal(io.icache.insts(io.icache.brIdx))

//     val brInst = io.icache.insts(io.icache.brIdx)
//     val brInstIndex = brInst(25 downto 0)
//     val brInstOpcode = brInst(31 downto 26)

//     val pcPlus4 = pc + 4
//     val redirectPc = UInt(32 bits)
//     redirectPc := 0
//     when (hasDirectJmp) {
//       redirectPc := pcPlus4(31 downto 28) @@ brInstIndex @@ U"00"
//     }
//     if2JumpInfo.jmpEn := hasDirectJmp
//     if2JumpInfo.jmpDest := redirectPc
//   }

// }

// object InstFetch {
//   def main(args: Array[String]) {
//     SpinalVerilog(new InstFetch)
//   }
// }