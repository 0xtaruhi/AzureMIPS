package azuremips.utils

import spinal.core._

object MuxLookup {
  def apply[S <: UInt, T <: Data](key: S, default: T, mapping: List[(S, T)]) = {
    val result = default
    for ((k, v) <- mapping) {
      when(key === k) {
        result := v
      }
    }
  }
}