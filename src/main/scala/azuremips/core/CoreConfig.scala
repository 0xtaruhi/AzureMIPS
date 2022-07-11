package azuremips.core

import spinal.core._

import azuremips.core.utils._
import azuremips.core.cache._
import azuremips.core.ifu._
import azuremips.core.idu._
import azuremips.core.rtu._

case class CoreConfig (
  exceptEntryPc: BigInt = 0xbfc00380,
  initPc: BigInt        = 0x10000000,
  icache: ICacheConfig  = ICacheConfig(),
  ifConfig: IFConfig    = IFConfig(),
  idConfig: IDConfig    = IDConfig(),
  dcache: DCacheConfig  = DCacheConfig(),
  rtConfig: RTConfig    = RTConfig(),
  robDepth: Int         = 8
) {
  require(robDepth % 2 == 0)
  val robIdxWidth = log2Up(robDepth)// 2 insts per entry
  val robRowNum   = robDepth / 2
}