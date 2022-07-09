package azuremips.core.ifu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class Bht(
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val updateInfo = new Bundle {
      val updateEn = in Bool()
      val updateTaken = in Bool()
    }
    val counter = out UInt(2 bits)
  }
  val a = TwoBitCounter()
  when (io.updateInfo.updateEn) {
    a.update(io.updateInfo.updateTaken)
  }
  io.counter := a.counter
}

object Bht extends App {
  SpinalVerilog(Bht())
}

class TwoBitCounter(initStatus: Int) {
  import TwoBitCounter._
  val counter = Reg(UInt(2 bits)) init(initStatus)
  def update(take: Bool) = {
    when (take) {
      switch (counter) {
        is (sStronglyTaken, sWeaklyTaken) { counter := sStronglyTaken }
        is (sWeaklyNotTaken) { counter := sWeaklyTaken }
        is (sStronglyNotTaken) { counter := sWeaklyNotTaken }
      }
    } otherwise {
      switch (counter) {
        is (sStronglyNotTaken, sWeaklyNotTaken) { counter := sStronglyNotTaken }
        is (sWeaklyTaken) { counter := sWeaklyNotTaken }
        is (sStronglyTaken) { counter := sWeaklyTaken }
      }
    }
  }
}

object TwoBitCounter {
  val sStronglyTaken = 0
  val sWeaklyTaken   = 1
  val sWeaklyNotTaken = 3
  val sStronglyNotTaken = 2

  def apply() = new TwoBitCounter(sWeaklyNotTaken)
}
