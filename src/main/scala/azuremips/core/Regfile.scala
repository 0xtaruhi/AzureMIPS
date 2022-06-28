package azuremips.core

import spinal.core._
import spinal.lib._

case class RegfileConfig(
  numRegisters: Int   = 32,
  numReadPorts: Int   = 6,
  numWritePorts: Int  = 2,
  dataWidth: Int      = 32,
  syncRead: Boolean   = true,
  resetValue: BigInt  = 0x0
) {
  val addrWidth = log2Up(numRegisters)
}

case class RegfileReadPort(config: RegfileConfig) extends Bundle with IMasterSlave {
  val addr = UInt(config.addrWidth bits)
  val data = UInt(config.dataWidth bits)
  override def asMaster(): Unit = {
    in(addr)
    out(data)
  }
}

case class RegfileWritePort(config: RegfileConfig) extends Bundle with IMasterSlave {
  val addr = UInt(config.addrWidth bits)
  val data = UInt(config.dataWidth bits)
  val wen  = Bool()
  override def asMaster(): Unit = {
    in(addr, data, wen)
  }
}

case class Regfile(config: RegfileConfig) extends Component {
  val io = new Bundle {
    val readPorts = Vec(master(RegfileReadPort(config)), config.numReadPorts)
    val writePorts = Vec(master(RegfileWritePort(config)), config.numWritePorts)
  }

  val regfile = Mem(UInt(config.dataWidth bits), config.numRegisters)
  for (readPort <- io.readPorts) {
    if (config.syncRead) {
      readPort.data := regfile.readSync(readPort.addr)
    } else {
      readPort.data := regfile.readAsync(readPort.addr)
    }
  }

  for (writePort <- io.writePorts) {
    when (writePort.wen) {
      regfile.write(writePort.addr, writePort.data)
    }
  }
}

object Regfile {
  def main(args: Array[String]) {
    SpinalVerilog(Regfile(RegfileConfig()))
  }
}