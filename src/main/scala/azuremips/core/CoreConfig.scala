package azuremips.core

import spinal.core._
import spinal.lib._

import azuremips.core.cache._
import azuremips.core.ifu._

case class CoreConfig(
  icache: ICacheConfig = ICacheConfig(),
  dcache: DCacheConfig = DCacheConfig(),
  ifConfig: IFConfig   = IFConfig()
) {
  val initPc = 0x0
}