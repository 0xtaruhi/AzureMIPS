package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

object BrMaskConsts {
  val brMaskWidth  = 2
  val BRMASK_NOP   = 0x0
  val BRMASK_J     = 0x1    // j or jal
  val BRMASK_JR    = 0x2    // jr or jalr
  val BRMASK_BX    = 0x3
}

class FastDecode extends Component {
  import BrMaskConsts._
  val io = new Bundle {
    val inst = in UInt(32 bits)
    val isBr = out Bool()
    val brMask = out UInt(brMaskWidth bits)
  }
  
  import Mips._

  val opcode = io.inst(opcodeRange)
  val rtAddr = io.inst(rtRange)

  val isJ = opcode === OP_J || opcode === OP_JAL
  val isBx = opcode === OP_BEQ || opcode === OP_BNE ||
            opcode === OP_BNE ||
            opcode === OP_BGTZ || opcode === OP_BLEZ ||
            opcode === OP_REGIMM && (
              // rtAddr === U"10001" || rtAddr === U"10000"
              rtAddr === RT_BLTZ || rtAddr === RT_BGEZ ||
              rtAddr === RT_BLTZAL || rtAddr === RT_BGEZAL
            )
  val isJr = io.inst === INST_JALR || io.inst === INST_JR
  io.isBr := isJ || isJr || isBx
  io.brMask := BRMASK_NOP
  when (isJ) {
    io.brMask := BRMASK_J
  }
  when (isJr) {
    io.brMask := BRMASK_JR
  }
  when (isBx) {
    io.brMask := BRMASK_BX
  }
}
