package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core._

case class DCacheConfig(
  tagWidth: Int = 20,
  indexWidth: Int = 6, // number of sets = 2 ** indexWidth
  bankIdxWidth: Int = 0, // (number of banks = 2 ** bankIdxWidth)
  idxWidth: Int = 2, // (number of ways = 2 ** idxWidth)
  zeroWidth: Int = 2, // word_t === 32

  cacheLineWidth: Int = 16, // mustn't change
  offsetWidth: Int = 4, // mustn't change
  portIdxWidth: Int = 1 // mustn't change, port num = 2 ** portWidth
) {
  // import Mips._
  // import AzureConsts._

  val wayNum = scala.math.pow(2, idxWidth).toInt
  val setNum = scala.math.pow(2, indexWidth).toInt
  val bankNum = scala.math.pow(2, bankIdxWidth).toInt
  val portNum = scala.math.pow(2, portIdxWidth).toInt
  val bankLineWidth: Int = cacheLineWidth / bankNum // num of words in one bank line
  val bankSize = bankLineWidth * wayNum * setNum
  val bankOffsetWidth = offsetWidth - bankIdxWidth // 4 mustn't change
  assert(bankOffsetWidth >= 0)
  val unusedBits = 32 - tagWidth - indexWidth - bankOffsetWidth - bankIdxWidth - zeroWidth
  assert(unusedBits >= 0)
  val selectWidth = wayNum - 1
  
  val selectRamWordWidth = selectWidth
  val tagRamWordWidth = tagWidth * wayNum
  val validRamWordWidth = 1 * wayNum // 1 bit for valid
  val dirtyRamWordWidth = 1 * wayNum // 1 bit for dirty
  val dataRamWordWidth = 32

  val dataAddrWidth = indexWidth + idxWidth + bankOffsetWidth

  val indexUpperBound = indexWidth + bankOffsetWidth + bankIdxWidth + zeroWidth - 1
  val indexLowerBound = bankOffsetWidth + bankIdxWidth + zeroWidth

  val tagLowerBound = indexUpperBound + 1
  val tagUpperBound = tagLowerBound + tagWidth - 1

  val offsetLowerBound = zeroWidth
  val offsetUpperBound = zeroWidth + offsetWidth - 1
}