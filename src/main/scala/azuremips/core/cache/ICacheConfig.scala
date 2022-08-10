package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core._

case class ICacheConfig(
  // |31 -unuse- 30|29 -TAG- 28|27 -unuse- 23|22 -TAG- 12|
  indexWidth: Int = 6, // number of sets = 2 ** indexWidth
  bankIdxWidth: Int = 2, // (number of banks = 2 ** bankIdxWidth)
  idxWidth: Int = 2, // (number of ways = 2 ** idxWidth)
  zeroWidth: Int = 2, // word_t === 32
  tagLoUpperBound: Int = 28,
  tagHiLowerBound: Int = 29,
  tagUpperBound: Int = 29,

  cacheLineWidth: Int = 16, // mustn't change
  offsetWidth: Int = 4, // mustn't change
  portIdxWidth: Int = 1 // (number of ports = 2 ** bankIdxWidth) mustn't change
) {
  // import Mips._
  // import AzureConsts._
  assert(indexWidth <= 6)
  val wayNum = scala.math.pow(2, idxWidth).toInt
  val setNum = scala.math.pow(2, indexWidth).toInt
  val bankNum = scala.math.pow(2, bankIdxWidth).toInt
  val portNum = scala.math.pow(2, portIdxWidth).toInt
  val bankLineWidth: Int = cacheLineWidth / bankNum // num of words in one bank line
  val bankSize = bankLineWidth * wayNum * setNum
  val bankOffsetWidth = offsetWidth - bankIdxWidth // 4 mustn't change
  assert(bankOffsetWidth >= 0)
  val selectWidth = wayNum - 1
  
  val selectRamWordWidth = selectWidth
  val validRamWordWidth = 1 * wayNum // 1 bit for valid
  val dataRamWordWidth = 32

  val dataAddrWidth = indexWidth + idxWidth + bankOffsetWidth

  val indexUpperBound = indexWidth + bankOffsetWidth + bankIdxWidth + zeroWidth - 1
  val indexLowerBound = bankOffsetWidth + bankIdxWidth + zeroWidth

  val tagLowerBound = indexUpperBound + 1
  val tagWidth = tagUpperBound + 1 - tagHiLowerBound + tagLoUpperBound + 1 - tagLowerBound
  val tagRamWordWidth = tagWidth * wayNum

  val unusedBits = 32 - tagWidth - indexWidth - bankOffsetWidth - bankIdxWidth - zeroWidth
  assert(unusedBits >= 1)

  val offsetLowerBound = zeroWidth
  val offsetUpperBound = zeroWidth + offsetWidth - 1
}