package azuremips.core.reg

import spinal.core._
import spinal.lib._

class WriteHiloRegfilePort extends Bundle with IMasterSlave {
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
    val hiloData = out UInt(64 bits)
    val write    = slave(new WriteHiloRegfilePort)
  }

  val hi = Reg(UInt(32 bits)) init (0)
  val lo = Reg(UInt(32 bits)) init (0)

  io.hiloData := hi @@ lo

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