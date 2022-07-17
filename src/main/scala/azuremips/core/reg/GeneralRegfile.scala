package azuremips.core.reg

import spinal.core._
import spinal.lib._

class ReadGeneralRegfilePort extends Bundle with IMasterSlave {
  val addr = UInt(5 bits)
  val data = UInt(32 bits)

  override def asMaster: Unit = {
    out(addr)
    in(data)
  }
}

class WriteGeneralRegfilePort extends Bundle with IMasterSlave {
  val wrEn = Bool()
  val addr = UInt(5 bits)
  val data = UInt(32 bits)

  def hit(addr: UInt): Bool = addr === this.addr && wrEn

  override def asMaster: Unit = {
    out(wrEn)
    out(addr)
    out(data)
  }
}

class GeneralRegfile extends Component {
  val io = new Bundle {
    val read = Vec(slave(new ReadGeneralRegfilePort), 4)
    val write = Vec(slave(new WriteGeneralRegfilePort), 2)
  }

  val regs = Vec(Reg(UInt(32 bits)) init(0), 32)

  for (readPort <- io.read) {
    when (readPort.addr === 0) {
      readPort.data := 0
    } otherwise {
      readPort.data := regs(readPort.addr)
      for (writePort <- io.write) {
        when (writePort.hit(readPort.addr)) {
          readPort.data := writePort.data
        }
      }
    }
  }

  for (writePort <- io.write) {
    when (writePort.wrEn && writePort.addr =/= 0) {
      regs(writePort.addr) := writePort.data
    }
  }
}

object GeneralRegfileVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new GeneralRegfile)
  }
}