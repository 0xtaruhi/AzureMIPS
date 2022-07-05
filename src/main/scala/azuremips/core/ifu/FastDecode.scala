package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

object BrMaskConsts {
  val brMaskWidth  = 2
  val BRMASK_NOP   = 0x0
  val BRMASK_J     = 0x1
  val BRMASK_JAL   = 0x2
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

  val isJ = opcode === OP_J
  val isJal = opcode === OP_JAL
  val isBx = opcode === OP_BEQ || opcode === OP_BNE ||
            opcode === OP_BNE || opcode === OP_BGEZ ||
            opcode === OP_BGTZ || opcode === OP_BLEZ ||
            opcode === U"000001" && (
              rtAddr === U"10001" || rtAddr === U"10000"
            ) || io.inst === INST_JR || io.inst === INST_JALR
  io.isBr := isJ || isJal || isBx
  io.brMask := BRMASK_NOP
  when (isJ) {
    io.brMask := BRMASK_J
  }
  when (isJal) {
    io.brMask := BRMASK_JAL
  }
  when (isBx) {
    io.brMask := BRMASK_BX
  }
}

object GenFastDecodeVerilog extends App {
  SpinalVerilog(new FastDecode)
}