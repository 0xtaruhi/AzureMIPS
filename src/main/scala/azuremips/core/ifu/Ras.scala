package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips._

case class RasEntry(config: IFConfig) extends Bundle {
  val addr = UInt(32 bits)
  val counter = UInt(config.rasCounterWidth bits)
}

case class Ras(config: IFConfig) extends Component {
  val io = new Bundle {
    val pushAddr = in UInt(32 bits)
    val pushEn   = in Bool()
    val popAddr  = out UInt(32 bits)
    val popEn    = in Bool()
  }

  val ras = Vec(Reg(RasEntry(config)), config.rasDepth)
  val tailPtr = Reg(UInt(log2Up(config.rasDepth) bits)) init(0)
  val headPtr = Reg(UInt(log2Up(config.rasDepth) bits)) init(0)
  
  for (entry <- ras) {
    entry.addr init(0)
    entry.counter init(0)
  }

  when (io.pushEn) {
    when (io.popEn) {
      io.popAddr := io.pushAddr
    } otherwise {
      when (ras(tailPtr).addr === io.pushAddr &&
          ras(tailPtr).counter =/= (1 << (config.rasCounterWidth - 1))) {
        ras(tailPtr).counter := ras(tailPtr).counter + 1    
      } otherwise {
        tailPtr := tailPtr + 1
        ras(tailPtr + 1).addr := io.pushAddr
        ras(tailPtr + 1).counter := 0
      }
    }
  } elsewhen (io.popEn) {
    io.popAddr := ras(tailPtr).addr
    tailPtr := tailPtr - 1
  }
}
