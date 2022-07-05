package azuremips.core.idu

import spinal.core._
import spinal.lib._

import azuremips.core._

class InstSignals extends Bundle {
  val valid         = Bool()
  val srcReg1Addr   = UInt(5 bits)
  val srcReg2Addr   = UInt(5 bits)
  val destRegWrEn   = Bool()
  val destRegWrAddr = UInt(5 bits)
  val immEn         = Bool()
  val immData       = UInt(32 bits)
  val useSa         = Bool()
  val excIOFValid   = Bool()
  val fu            = UInt(AzureConsts.fuWidth bits)
  val uop           = UInt(AzureConsts.uopWidth bits)
}

case class Decoder(config: CoreConfig) extends Component {
  val io = new Bundle {
    val valid   = in Bool()
    val inst    = in UInt(32 bits)
    val signals = out(new InstSignals)
  }

  import Mips._
  import Uops._
  import FU._

  io.signals.valid := False
  io.signals.srcReg1Addr := U(0)
  io.signals.srcReg2Addr := U(0)
  io.signals.destRegWrAddr := U(0)
  io.signals.destRegWrEn := False
  io.signals.immEn       := False
  io.signals.immData     := 0
  io.signals.uop         := UOP_NOP
  io.signals.fu          := FU_ALU
  io.signals.excIOFValid := False
  io.signals.useSa       := False
  
  def signExt(x: UInt, width: Int = 32) = {
    if (x.getWidth < width) {
      S(x).resize(width).asUInt
    } else { x }
  }

  def zeroExt(x: UInt, width: Int = 32) = {
    if (x.getWidth < width) {
      x.resize(width)
    } else { x }
  }

  def rsAddr = io.inst(rsRange)
  def rtAddr = io.inst(rtRange)
  def rdAddr = io.inst(rdRange)
  def imm    = io.inst(immediateRange)
  def opcode = io.inst(opcodeRange)
  def funct  = io.inst(functRange)

  switch (opcode) {
    for (immOp <- immInstOpList) yield {
      is (immOp) {
        io.signals.valid := True
        io.signals.uop := opcodeUopMap(immOp)
        io.signals.immEn := True
        io.signals.immData := {
          if (immOp == OP_ORI || immOp == OP_XORI || immOp == OP_ANDI) {
            zeroExt(imm)
          } else {
            signExt(imm)
          }
        }
        io.signals.srcReg1Addr := rsAddr
        io.signals.destRegWrEn := True
        io.signals.destRegWrAddr := rtAddr
        io.signals.excIOFValid := {
          if (immOp == OP_ADDI) True else False
        }
        io.signals.fu := FU_ALU
      }
    }
    is (OP_SPEC) {
      switch (funct) {
        for (specFunct <- specInstFunctList) yield {
          is (specFunct) {
            io.signals.valid := True
            io.signals.uop := specFunctUopMap(specFunct)
            // io.signals.srcReg1Addr := 
          }
        }
      }
    }
  }

}

object genDecodeVerilog extends App {
  SpinalVerilog(Decoder(CoreConfig()))
}