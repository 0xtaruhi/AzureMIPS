package azuremips.core.idu

import spinal.core._
import spinal.lib._

class BranchDecoder extends Component {
  val io = new Bundle {
    val inst              = in  UInt(32 bits)
    val isImmDirectJump   = out Bool()
    val branchImm         = out UInt(32 bits)
    val jumpImm           = out UInt(32 bits)
    val isRegDirectJump   = out Bool()
    val isBranch          = out Bool()
    val isReturn          = out Bool()
    val isCall            = out Bool()
    val isBrOrJmp         = out Bool()
  }

  val opcode = io.inst(31 downto 26)
  val rs     = io.inst(25 downto 21)
  val rt     = io.inst(20 downto 16)
  val rd     = io.inst(15 downto 11)
  val funct  = io.inst(5  downto  0)

  io.jumpImm   := U"0000" @@ io.inst(25 downto 0) @@ U"00"
  io.branchImm := U(0, 14 bits) @@ io.inst(15 downto 0) @@ U"00"

  io.isRegDirectJump := opcode === 0 && funct(5 downto 1) === U"00100"
  io.isImmDirectJump := opcode(5 downto 1) === U"00001"

  io.isBranch := (opcode(5 downto 2) === U"0001") ||
                 (opcode === U"000001" && io.inst(19 downto 17) === U"000")
  
  io.isCall   := // io.isRegDirectJump && rd === 31 || 
                 opcode === U"000011" // ||
                // opcode === U"000001" && io.inst(20 downto 17) === U"1000"
  
  io.isReturn := opcode === 0 && funct === U"001000" && rs === 31 // || io.isRegDirectJump && rs === 31
  
  io.isBrOrJmp := io.isImmDirectJump || io.isRegDirectJump || io.isBranch
}

object GenerateBranchDecoder extends App {
  SpinalVerilog(new BranchDecoder)
}