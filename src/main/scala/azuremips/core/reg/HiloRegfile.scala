package azuremips.core.reg

import spinal.core._
import spinal.lib._

// class ReadHiloRegfilePort extends Bundle with IMasterSlave {
//   val isHi   = Bool()
//   val data   = UInt(32 bits)

//   override def asMaster: Unit = {
//     out(isHi)
//     in(data)
//   }
// }

class WriteHiloRegfilePort extends Bundle with IMasterSlave {
  val wrEn    = Bool()
  val isHi    = Bool()
  val data    = UInt(32 bits)

  override def asMaster: Unit = {
    out(wrEn)
    out(isHi)
    out(data)
  }
}

class HiloRegfile extends Component {
  val io = new Bundle {
    // val read  = Vec(slave(new ReadHiloRegfilePort), 2)
    val hiloData = out UInt(64 bits)
    val write = Vec(slave(new WriteHiloRegfilePort), 2)
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

  for (writePort <- io.write) {
    when (writePort.wrEn && writePort.isHi) {
      hi := writePort.data
    }
    when (writePort.wrEn && !writePort.isHi) {
      lo := writePort.data
    }
  }
}

object GenHiloRegfileVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new HiloRegfile)
  }
}