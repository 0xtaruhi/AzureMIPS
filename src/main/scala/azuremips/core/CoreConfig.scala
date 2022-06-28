package azuremips.core

import spinal.core._

case class CoreConfig (
  exceptEntryPc: BigInt = 0xbfc00380,
  initPc: BigInt        = 0x10000000,
  vaddrWidth: BigInt    = 32,
  paddrWidth: BigInt    = 32,
  regfileConfig: RegfileConfig = RegfileConfig()
)