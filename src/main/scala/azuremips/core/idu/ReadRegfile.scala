package azuremips.core.idu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.reg.ReadGeneralRegfilePort

case class ReadRfSignals() extends Bundle {
  val validInst  = Bool()
  val pc         = UInt(32 bits)
  val op1Data    = UInt(32 bits)
  val op2Data    = UInt(32 bits)
  val wrRegAddr  = UInt(5 bits)
  val wrRegEn    = Bool()
  val uop        = Uops()
  val imm        = UInt(32 bits)
  val isPriv     = Bool()
  val multiCycle = Bool()
  val isBr       = Bool()

  def nopReadRfSignals = {
    val r = new ReadRfSignals
    r.validInst  := False
    r.pc         := 0
    r.op1Data    := 0
    r.op2Data    := 0
    r.wrRegAddr  := 0
    r.wrRegEn    := False
    r.uop        := uOpSll
    r.imm        := 0
    r.isPriv     := False
    r.multiCycle := False
    r.isBr       := False
    r
  }
}

case class ReadRegfiles() extends Component {
  val io = new Bundle {
    val flush          = in Bool()
    val decodedSignals = Vec(in(new DecodedSignals), 2)
    val readrfSignals  = Vec(out(new ReadRfSignals), 2)
    val generalRegfile = Vec(master(new ReadGeneralRegfilePort), 4)

    val exBypass       = Vec(in(new BypassPort), 2)
    val mem1Bypass     = Vec(in(new BypassPort), 2)
    val mem2Bypass     = Vec(in(new BypassPort), 2)
    val mem3Bypass     = Vec(in(new BypassPort), 2)

    val loadRawStall   = out Bool()
  }

  val units = for (i <- 0 until 2) yield new SingleReadRegfile()
  
  for (i <- 0 until 2) {
    units(i).io.decodedSignals := io.decodedSignals(i)
    units(i).io.exBypass       := io.exBypass
    units(i).io.mem1Bypass     := io.mem1Bypass
    units(i).io.mem2Bypass     := io.mem2Bypass
    units(i).io.mem3Bypass     := io.mem3Bypass

    units(i).io.generalRegfile(0) <> io.generalRegfile(2 * i + 0)
    units(i).io.generalRegfile(1) <> io.generalRegfile(2 * i + 1)
  }

  io.loadRawStall := units.map(_.io.loadRawStall).reduce(_ || _) && !io.flush
  val flushDeEn = RegNext(io.flush)
  for (i <- 0 until 2) {
    io.readrfSignals(i) := units(i).io.readrfSignals
    when (io.flush || io.loadRawStall || RegNext(flushDeEn) || flushDeEn) { // flush buf/de, de/is, rf/iss
      io.readrfSignals(i) := new ReadRfSignals().nopReadRfSignals
    }
  }
}

case class SingleReadRegfile() extends Component {
  val io = new Bundle {
    val decodedSignals = in(new DecodedSignals)
    val readrfSignals  = out(new ReadRfSignals)
    val generalRegfile = Vec(master(new ReadGeneralRegfilePort), 2)

    // bypass
    val exBypass   = Vec(in(new BypassPort), 2)
    val mem1Bypass = Vec(in(new BypassPort), 2)
    val mem2Bypass = Vec(in(new BypassPort), 2)
    val mem3Bypass = Vec(in(new BypassPort), 2)

    val loadRawStall  = out Bool()
  }

  io.readrfSignals.validInst  := io.decodedSignals.validInst
  io.generalRegfile(0).addr   := io.decodedSignals.op1Addr
  io.generalRegfile(1).addr   := io.decodedSignals.op2Addr
  io.readrfSignals.pc         := io.decodedSignals.pc
  io.readrfSignals.wrRegAddr  := io.decodedSignals.wrRegAddr
  io.readrfSignals.uop        := io.decodedSignals.uop
  io.readrfSignals.imm        := io.decodedSignals.imm
  io.readrfSignals.wrRegEn    := io.decodedSignals.wrRegEn
  io.readrfSignals.isPriv     := io.decodedSignals.isPriv
  io.readrfSignals.multiCycle := io.decodedSignals.multiCycle
  io.readrfSignals.isBr       := io.decodedSignals.isBr

  val loadRawStallOp1 = False
  val loadRawStallOp2 = False

  when (io.exBypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.exBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp1 := io.exBypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem1Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem1Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp1 := io.mem1Bypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem2Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem2Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp1 := io.mem2Bypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem3Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem3Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
  } otherwise {
    io.readrfSignals.op1Data := io.generalRegfile(0).data
  }

  when (io.exBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.exBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp2 := io.exBypass.map(_.stall(io.decodedSignals.op2Addr)).reduce(_ || _)
  } elsewhen (io.mem1Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem1Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp2 := io.mem1Bypass.map(_.stall(io.decodedSignals.op2Addr)).reduce(_ || _)
  } elsewhen (io.mem2Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem2Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    loadRawStallOp2 := io.mem2Bypass.map(_.stall(io.decodedSignals.op2Addr)).reduce(_ || _)
  } elsewhen (io.mem3Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem3Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
  } otherwise {
    io.readrfSignals.op2Data := io.generalRegfile(1).data
  }

  io.loadRawStall := loadRawStallOp1 || loadRawStallOp2

  when (io.decodedSignals.useImm) {
    io.readrfSignals.op2Data := io.readrfSignals.imm
  }
}

object GenReadRegfileVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new ReadRegfiles)
  }
}