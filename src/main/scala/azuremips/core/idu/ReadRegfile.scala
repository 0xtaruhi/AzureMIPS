package azuremips.core.idu

import spinal.core._
import spinal.lib._
import azuremips.core._
import azuremips.core.Uops._
// import azuremips.core.reg.{ReadGeneralRegfilePort, ReadHiloRegfilePort}
import azuremips.core.reg.ReadGeneralRegfilePort

class ReadRfSignals extends Bundle {
  val validInst  = Bool()
  val pc         = UInt(32 bits)
  val op1Data    = UInt(32 bits)
  val op2Data    = UInt(32 bits)
  // val hiloData   = UInt(64 bits)
  val wrRegAddr  = UInt(5 bits)
  val wrRegEn    = Bool()
  val uop        = Uops()
  val imm        = UInt(32 bits)
  // val isLoad     = Bool()
  // val isStore    = Bool()
  val isPriv     = Bool()
  val multiCycle = Bool()
}

case class ReadRegfile() extends Component {
  val io = new Bundle {
    val decodedSignals = in(new DecodedSignals)
    val readrfSignals  = out(new ReadRfSignals)
    val generalRegfile = Vec(master(new ReadGeneralRegfilePort), 2)

    val exBypass  = Vec(in(new BypassPort), 2)
    val mem1Bypass = Vec(in(new BypassPort), 2)
    val mem2Bypass = Vec(in(new BypassPort), 2)
    val mem3Bypass = Vec(in(new BypassPort), 2)

    val loadRawStall  = out Bool()
  }

  io.generalRegfile(0).addr := io.decodedSignals.op1Addr
  io.generalRegfile(1).addr := io.decodedSignals.op2Addr
  // io.readrfSignals.hiloData := io.hiloData
  io.readrfSignals.pc := io.decodedSignals.pc
  io.readrfSignals.wrRegAddr := io.decodedSignals.wrRegAddr
  io.readrfSignals.uop := io.decodedSignals.uop
  io.readrfSignals.imm := io.decodedSignals.imm
  io.readrfSignals.wrRegEn := io.decodedSignals.wrRegEn
  // io.readrfSignals.isLoad := io.decodedSignals.isLoad
  // io.readrfSignals.isStore := io.decodedSignals.isStore
  io.readrfSignals.isPriv := io.decodedSignals.isPriv
  io.readrfSignals.multiCycle := io.decodedSignals.multiCycle

  io.loadRawStall := False


  when (io.exBypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.exBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := io.exBypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem1Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem1Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := io.mem1Bypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem2Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem2Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := io.mem2Bypass.map(_.stall(io.decodedSignals.op1Addr)).reduce(_ || _)
  } elsewhen (io.mem3Bypass.map(_.hit(io.decodedSignals.op1Addr)).reduce(_ || _)) {
    io.readrfSignals.op1Data := io.mem3Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op1Addr), bp.wrData, U(0)) }.reduce(_ | _)
  } otherwise {
    io.readrfSignals.op1Data := io.generalRegfile(0).data
  }

   when (io.exBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.exBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := Mux(io.exBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _), True, False)
  } elsewhen (io.mem1Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem1Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := io.mem1Bypass.map(_.stall(io.decodedSignals.op2Addr)).reduce(_ || _)
  } elsewhen (io.mem2Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem2Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
    io.loadRawStall := io.mem2Bypass.map(_.stall(io.decodedSignals.op2Addr)).reduce(_ || _)
  } elsewhen (io.mem3Bypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
    io.readrfSignals.op2Data := io.mem3Bypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
  } otherwise {
    io.readrfSignals.op2Data := io.generalRegfile(1).data
  }

  // when (io.exBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
  //   io.readrfSignals.op2Data := io.exBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
  // } elsewhen (io.memBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
  //   io.readrfSignals.op2Data := io.memBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
  // } elsewhen (io.wbBypass.map(_.hit(io.decodedSignals.op2Addr)).reduce(_ || _)) {
  //   io.readrfSignals.op2Data := io.wbBypass.map{ bp => Mux(bp.hit(io.decodedSignals.op2Addr), bp.wrData, U(0)) }.reduce(_ | _)
  // } otherwise {
  //   io.readrfSignals.op2Data := io.generalRegfile(1).data
  // }
}

object GenReadRegfileVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new ReadRegfile)
  }
}