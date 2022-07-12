package azuremips.core.idu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class InstSignals(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val valid         = Bool()
  val pc            = UInt(32 bits)
  val srcReg1Addr   = UInt(5 bits)  
  val srcReg2Addr   = UInt(5 bits)
  val destRegWrEn   = Bool()
  val destRegWrAddr = UInt(5 bits)
  val immEn         = Bool()
  val immData       = UInt(32 bits)
  val offset        = UInt(16 bits)
  val sel           = UInt(3 bits)
  // val excIOFValid   = Bool()
  // val excBP         = Bool()        // Breakpoint
  // val excSC         = Bool()        // SystemCall
  val fu            = UInt(AzureConsts.fuWidth bits)
  val uop           = UInt(AzureConsts.uopWidth bits)
}

case class Decoder(config: CoreConfig) extends Component {
  val io = new Bundle {
    // val valid   = in Bool()
    val inst    = in UInt(32 bits)
    val pc      = in UInt(32 bits)
    // val mdStage = in UInt()   // 0 for stage1(write LO); 1 for stage2(write HI)
    val signals = out(InstSignals(config))
  }

  import Mips._
  import Uops._
  import FU._

  io.signals.valid := False
  io.signals.pc := io.pc
  io.signals.srcReg1Addr := U(0)
  io.signals.srcReg2Addr := U(0)
  io.signals.destRegWrAddr := U(0)
  io.signals.destRegWrEn := False
  io.signals.immEn       := False
  io.signals.immData     := 0
  io.signals.offset      := U(0)
  io.signals.sel         := U(0)
  io.signals.uop         := UOP_NOP
  io.signals.fu          := FU_ALU
  // io.signals.excIOFValid := False
  // io.signals.excBP       := False
  // io.signals.excSC       := False
  
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
  def sa     = io.inst(saRange)
  def imm    = io.inst(immediateRange)
  def opcode = io.inst(opcodeRange)
  def funct  = io.inst(functRange)
  def sel    = io.inst(selRange)

  switch (opcode) {
    for (immOp <- immInstOpList) yield {
      is (immOp) {
        io.signals.valid := True
        io.signals.uop := immOpUopMap(immOp)
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
        // io.signals.excIOFValid := {
        //   if (immOp == OP_ADDI) True else False
        // }
        io.signals.fu := FU_ALU
      }
    }
    for (brOp <- brInstOpList) yield {
      is (brOp) {
        io.signals.valid := True
        io.signals.srcReg1Addr := rsAddr
        // io.signals.srcReg2Addr := {
        //   if (brOp == OP_BNE || brOp == OP_BEQ) {
        //     rtAddr
        //   } else {
        //     0
        //   }
        // }
        if (brOp == OP_BNE || brOp == OP_BEQ) {
          io.signals.srcReg2Addr := rtAddr
        } else {
          io.signals.srcReg2Addr := 0
        }
        io.signals.offset := imm
        io.signals.uop := brOpUopMap(brOp)
        io.signals.fu := FU_ALU
      }
    }
    for (loadOp <- loadInstOpList) yield {
      is(loadOp) {
        io.signals.valid := True
        io.signals.srcReg1Addr := rsAddr
        io.signals.destRegWrEn := True
        io.signals.destRegWrAddr := rtAddr
        io.signals.immEn := True  // here offset in imm
        io.signals.immData := signExt(imm)
        io.signals.uop := loadOpUopMap(loadOp)
        io.signals.fu := FU_LSU
      }
    }
    for (stoOp <- stoInstOpList) yield {
      is (stoOp) {
        io.signals.valid := True
        io.signals.srcReg1Addr := rsAddr  // base
        io.signals.srcReg2Addr := rtAddr  // data source
        io.signals.immEn := True  // here offset in imm
        io.signals.immData := signExt(imm)
        io.signals.uop := stoOpUopMap(stoOp)
        io.signals.fu := FU_LSU
      }
    }
    is (OP_SPEC) {
      switch (funct) {
        for (specArLoFunct <- specArLoInstFunctList) yield {
          is (specArLoFunct) {
            io.signals.valid := True
            io.signals.uop := specArLoFunctUopMap(specArLoFunct)
            io.signals.srcReg1Addr := rsAddr
            io.signals.srcReg2Addr := rtAddr
            io.signals.destRegWrEn := True
            io.signals.destRegWrAddr := rdAddr
            // io.signals.excIOFValid := {
            //   if (specArLoFunct == FUN_ADD || specArLoFunct == FUN_SUB) True else False
            // }
            io.signals.fu := FU_ALU
          }
        }
        for (specShSaFunct <- specShSaInstFunctList) yield {
          is (specShSaFunct) {
            io.signals.valid := True
            io.signals.uop := specShSaFunctUopMap(specShSaFunct)
            io.signals.srcReg1Addr := rtAddr
            io.signals.immEn := True
            io.signals.immData := sa.resize(32)
            io.signals.destRegWrEn := True
            io.signals.destRegWrAddr := rtAddr
            io.signals.fu := FU_ALU
          }
        }
        for (specShVFunct <- specShVInstFunctList) yield {
          is (specShVFunct) {
            io.signals.valid := True
            io.signals.uop := specShVFunctUopMap(specShVFunct)
            io.signals.srcReg1Addr := rtAddr
            io.signals.srcReg2Addr := rsAddr
            io.signals.destRegWrEn := True
            io.signals.destRegWrAddr := rtAddr
            io.signals.fu := FU_ALU
          }
        }
        for (specMDFunct <- specMDInstFunctList) yield {
          is (specMDFunct) {
            io.signals.valid := True
            io.signals.uop := specMDFunctUopMap(specMDFunct)
            io.signals.srcReg1Addr := rsAddr
            io.signals.srcReg2Addr := rtAddr
            io.signals.destRegWrEn := True
            // io.signals.destRegWrAddr := {
            //   if (io.mdStage) 0x21 else 0x20
            // }
            // io.signals.destRegWrAddr := Mux(io.mdStage === 1, 0x21, 0x20)
            io.signals.fu := FU_MD
          }
        }
        is (FUN_JR){
          io.signals.valid := True
        }
        is (FUN_JALR){
          io.signals.valid := True
          io.signals.destRegWrEn := True
          io.signals.destRegWrAddr := rdAddr
          io.signals.uop := ALU_JAL
          io.signals.fu := FU_ALU
        }
        is (FUN_MFHI){
          io.signals.valid := True
          // io.signals.srcReg1Addr := 0x21
          io.signals.destRegWrEn := True
          io.signals.destRegWrAddr := rdAddr
          io.signals.immEn := True
          io.signals.immData := 0
          io.signals.uop := ALU_ADD
          io.signals.fu := FU_ALU
        }
        is (FUN_MFLO){
          io.signals.valid := True
          // io.signals.srcReg1Addr := 0x20
          io.signals.destRegWrEn := True
          io.signals.destRegWrAddr := rdAddr
          io.signals.immEn := True
          io.signals.immData := 0
          io.signals.uop := ALU_ADD
          io.signals.fu := FU_ALU
        }
        is (FUN_MTHI){
          io.signals.valid := True
          io.signals.srcReg1Addr := rsAddr
          io.signals.immEn := True
          io.signals.immData := 0
          io.signals.uop := ALU_ADD
          io.signals.fu := FU_ALU
        }
        is (FUN_MTLO){
          io.signals.valid := True
          io.signals.srcReg1Addr := rsAddr
          io.signals.immEn := True
          io.signals.immData := 0
          io.signals.uop := ALU_ADD
          io.signals.fu := FU_ALU
        }
        is (FUN_BREAK){
          io.signals.valid := True
          io.signals.uop := UOP_NOP
          io.signals.fu := FU_ALU
        }
        is (FUN_SYSCALL){
          io.signals.valid := True
          io.signals.uop := UOP_NOP
          io.signals.fu := FU_ALU
        }
      }
    }
    is (OP_REGIMM) {
      switch (rtAddr){
        for (regImmRt <- regImmInstRtList) yield {
          is(regImmRt) {
            io.signals.valid := True
            io.signals.uop := regImmRtUopMap(regImmRt)
            io.signals.srcReg1Addr := rsAddr
            if (regImmRt == RT_BLTZAL || 
                regImmRt == RT_BGEZAL) {
              io.signals.destRegWrEn := True
              io.signals.destRegWrAddr := 31
            } else {
              io.signals.destRegWrEn := False
              io.signals.destRegWrAddr := 0
            }
            io.signals.offset := imm
            io.signals.fu := FU_ALU
          }
        }
      }
    }
    is (OP_J){
      io.signals.valid := True
      io.signals.fu := FU_ALU
      io.signals.uop := UOP_NOP
    }
    is (OP_JAL) {
      io.signals.valid := True
      io.signals.destRegWrEn := True
      io.signals.destRegWrAddr := 0x1F
      io.signals.fu := FU_ALU
      io.signals.uop := ALU_JAL
    }
    is (OP_PRIV) {
      switch (rsAddr){
        is (RS_ERET){
          io.signals.valid := True
          io.signals.uop   := UOP_NOP
          io.signals.fu    := FU_ALU
        }
        is (RS_MFC0){
          io.signals.valid := True
          io.signals.srcReg1Addr := rdAddr
          io.signals.sel := sel
          io.signals.destRegWrEn := True
          io.signals.destRegWrAddr := rtAddr
          io.signals.uop := ALU_CP0
          io.signals.fu := FU_ALU
        }
        is (RS_MTC0){
          io.signals.valid := True
          io.signals.srcReg1Addr := rdAddr  // CP0 addr
          io.signals.srcReg2Addr := rtAddr  // data source addr
          io.signals.sel := sel
          io.signals.uop := ALU_CP0
          io.signals.fu := FU_ALU
        }
      }
    }
  }
}

object Decoder {
  def apply(inst: UInt): InstSignals = {
    val decoder = Decoder(CoreConfig())
    decoder.io.inst := inst
    decoder.io.signals
  }
}

object GenDecoderVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(Decoder(CoreConfig()))
  }
}