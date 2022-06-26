package azuremips.core

import spinal.core._

import azuremips.core.utils._
import azuremips.core.cache._

case class CoreConfig (
  exceptEntryPc: BigInt = 0xbfc00380,
  initPc: BigInt        = 0x10000000,
  vaddrWidth: BigInt    = 32,
  paddrWidth: BigInt    = 32,
  cache: CacheConfig    = CacheConfig()
)