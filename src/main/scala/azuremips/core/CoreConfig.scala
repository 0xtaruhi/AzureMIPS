package azuremips.core

import spinal.core._

import azuremips.core.utils._
import azuremips.core.cache._
import azuremips.core.ifu._
import azuremips.core.idu._
import azuremips.core.exu._
import azuremips.core.rtu._
import azuremips.core.ooo._

case class CoreConfig (
  exptEntryPc: BigInt   = 0xbfc00380,
  initPc: BigInt        = 0x00000000,
  icache: ICacheConfig  = ICacheConfig(),
  ifConfig: IFConfig    = IFConfig(),
  idConfig: IDConfig    = IDConfig(),
  dcache: DCacheConfig  = DCacheConfig(),
  exConfig: EXConfig    = EXConfig(),
  rtConfig: RTConfig    = RTConfig(),
  oooConfig: OOOConfig  = OOOConfig(),
  robDepth: Int         = 64
) {
  require(robDepth % 2 == 0)
  // val robIdxWidth = log2Up(robDepth)// 2 insts per entry
  // val robRowNum   = robDepth / 2 // Dual
  val robAddrWidth = log2Up(robDepth) + 1 // consider position bit
}