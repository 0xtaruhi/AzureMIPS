package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class JumpInfo(config: CoreConfig) extends Bundle{
  val jmpEn = Bool()
  val jmpDest = UInt(32 bits)
}

class InstFetch(
  val config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val ifJmp = in(JumpInfo(config))
  }
  val curPc = Reg(UInt(32 bits)) init (config.initPc)
  
  val jmpFlag = io.ifJmp.jmpEn
  val jmpDest = io.ifJmp.jmpDest

  curPc := jmpFlag ? jmpDest | (curPc + 4)
}

object InstFetch {
  def main(args: Array[String]) {
    SpinalVerilog(new InstFetch)
  }
}