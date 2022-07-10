package azuremips.core.utils

import spinal.core._
import spinal.lib._

object Log2 {
  def apply(x: UInt, width: Int): UInt = {
    if (width < 2) {
      U(0)
    } else if (width == 2) {
      U(1)
    } else if (width <= divideAndConquerThreshold) {
      Mux(x(width - 1), U(width - 1), apply(x, width - 1))
    } else {
      val mid = 1 << (log2Up(width) - 1)
      val hi = x((width - 1) downto mid)
      val lo = x((mid - 1) downto 0)
      val useHi = hi.orR
      U(useHi) @@ Mux(useHi, Log2(hi, width - mid), Log2(lo, mid))
    }
  }

  def apply(x: UInt): UInt = apply(x, x.getWidth)

  private def divideAndConquerThreshold = 4
}