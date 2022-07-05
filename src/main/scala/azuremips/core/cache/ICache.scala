package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core.ifu.IF2ICache
import azuremips.core._

case class ICache(config: CoreConfig) extends Component {
  val io = slave(IF2ICache(config))

  // val cache = Vec(Mem(UInt(32 bits), config.icache.sizePerBank), bankNum)
  io.hasBr := False
  io.brIdx := 2
  io.insts := Vec(U(0), 4)
  // io.insts := Vec(cache(0).)
}

object ICache {
  def main(args: Array[String]) {
    SpinalVerilog(ICache(CoreConfig()))
  }
}