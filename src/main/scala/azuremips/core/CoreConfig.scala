package azuremips.core

import spinal.core._

import azuremips.core.utils._
import azuremips.core.cache._
import azuremips.core.ifu._

case class CoreConfig (
  exceptEntryPc: BigInt = 0xbfc00380,
  initPc: BigInt        = 0x10000000,
  vaddrWidth: BigInt    = 32,
  paddrWidth: BigInt    = 32,
  icache: ICacheConfig  = ICacheConfig(),
  ifConfig: IFConfig    = IFConfig()
)