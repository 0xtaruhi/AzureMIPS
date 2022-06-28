package azuremips.core

import spinal.core._
import spinal.lib._

// import azuremips.core.Mips._

case class Decoder(config: CoreConfig) extends Component {
  val io = new Bundle {
    val inst       = in UInt(32 bits)
    val valid      = out Bool()
    val uop        = out UInt(Uops.uopWidth bits)
    val wenRegfile = out Bool()
    val wenMem     = out Bool()
    val rdMem      = out Bool()
    val br         = out Bool()
  }

  io.uop := 0
  io.wenRegfile := False
  io.wenMem := False
  io.rdMem := False
  io.br := False

  switch (io.inst) {
    is (Mips.ADD, Mips.ADDI, Mips.ADDU, Mips.ADDIU) {
      io.uop := Uops.ALU_ADD
      io.wenRegfile := True
    }
    is (Mips.SUB, Mips.SUBU) {
      io.uop := Uops.ALU_SUB
      io.wenRegfile := True
    }
    is (Mips.SLT, Mips.SLTI) {
      io.uop := Uops.ALU_SLT
      io.wenRegfile := True
    }
    is (Mips.NOR) {
      io.uop := Uops.ALU_NOR
      io.wenRegfile := True
    }
    is (Mips.AND, Mips.ANDI) {
      io.uop := Uops.ALU_AND
      io.wenRegfile := True
    }
    is (Mips.OR, Mips.ORI) {
      io.uop := Uops.ALU_OR
      io.wenRegfile := True
    }
    is (Mips.SLL, Mips.SLLV) {
      io.uop := Uops.ALU_SLL
      io.wenRegfile := True
    }
    is (Mips.SRL, Mips.SRLV) {
      io.uop := Uops.ALU_SRL
      io.wenRegfile := True
    }
  }
}

object Decoder {
  def apply(inst: UInt) = {
    val decoder = new Decoder(CoreConfig())
    decoder.io.inst := inst
    decoder
  }
}

case class BypassBundle(config: CoreConfig) extends Bundle {
  val en = in Bool()
  val addr = in UInt(config.regfileConfig.addrWidth bits)
  val data = in UInt(config.regfileConfig.dataWidth bits)
}

case class InstDecode(config: CoreConfig) extends Component {
  val io = new Bundle {
    val inst      = in UInt(32 bits)
    val exBypass  = in(BypassBundle(config))
    val memBypass = in(BypassBundle(config))
    val regfile   = Vec(slave(RegfileReadPort(config.regfileConfig)), 2)
    val op1       = out UInt(32 bits)
    val op2       = out UInt(32 bits)
    val uop       = out(UInt(Uops.uopWidth bits))
  }
  
  def bypassHit(bypass: BypassBundle, addr: UInt) = {
    val hit = bypass.en && (bypass.addr === addr)
    hit
  }

  val rsAddr = io.inst(Mips.rsRange)
  val rtAddr = io.inst(Mips.rtRange)
  val op1 = UInt(32 bits)
  val op2 = UInt(32 bits)

  val decoder = Decoder(io.inst)
  io.uop := decoder.io.uop
  io.regfile(0).addr := rsAddr
  io.regfile(1).addr := rtAddr

  op1 := Mux(bypassHit(io.memBypass, rsAddr), io.memBypass.data, 
            Mux(bypassHit(io.exBypass, rsAddr), io.exBypass.data, io.regfile(0).data))
  op2 := Mux(bypassHit(io.memBypass, rtAddr), io.memBypass.data,
            Mux(bypassHit(io.exBypass, rtAddr), io.exBypass.data, io.regfile(1).data))
  
  io.op1 := op1
  io.op2 := op2
}

object InstDecode {
  def main(args: Array[String]) {
    SpinalVerilog(InstDecode(CoreConfig()))
  }
}