package azuremips.core.cache

import spinal.core._

case class ICacheConfig(
  bankNum: Int = 4,
  totalSize: BigInt = 4 kB
) {
  require(isPow2(bankNum))
  val sizePerBank = totalSize / bankNum
}