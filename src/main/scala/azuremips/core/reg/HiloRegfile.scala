package azuremips.core.reg

import spinal.core._
import spinal.lib._

class WriteHiloRegfilePort extends Bundle with IMasterSlave {
  // val wrEn    = Bool()
  // val isHi    = Bool()
  // val data    = UInt(32 bits)

  // override def asMaster: Unit = {
  //   out(wrEn)
  //   out(isHi)
  //   out(data)
  // }
  val wrHi = Bool()
  val wrLo = Bool()
  val hiData = UInt(32 bits)
  val loData = UInt(32 bits)

  override def asMaster : Unit = {
    out (wrHi, wrLo, hiData, loData)
  }
}

class HiloRegfile extends Component {
  val io = new Bundle {
    // val read  = Vec(slave(new ReadHiloRegfilePort), 2)
    val hiloData = out UInt(64 bits)
    // val write = Vec(slave(new WriteHiloRegfilePort), 2)
    val write = slave(new WriteHiloRegfilePort)
  }

  val hi = Reg(UInt(32 bits)) init (0)
  val lo = Reg(UInt(32 bits)) init (0)

  // for (readPort <- io.read) {
  //   readPort.data := Mux(readPort.isHi, hi, lo)
  //   for (writePort <- io.write) {
  //     when (writePort.wrEn && writePort.isHi === readPort.isHi) {
  //       readPort.data := writePort.data
  //     }
  //   }
  // }
  io.hiloData := hi @@ lo

  // for (writePort <- io.write) {
  //   when (writePort.wrEn && writePort.isHi) {
  //     hi := writePort.data
  //   }
  //   when (writePort.wrEn && !writePort.isHi) {
  //     lo := writePort.data
  //   }
  // }
  when (io.write.wrHi) {
    hi := io.write.hiData
  }
  when (io.write.wrLo) {
    lo := io.write.loData
  }
}

object GenHiloRegfileVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new HiloRegfile)
  }
}