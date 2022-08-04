package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._

object TwoBitCounterStatus extends SpinalEnum {
  val StronglyNotTaken = 0
  val WeaklyNotTaken   = 1
  val WeaklyTaken      = 3
  val StronglyTaken    = 2
}

case class TwoBitCounter() extends Bundle {
  import TwoBitCounterStatus._
  val status = UInt(2 bits)

  def update(jump : Bool) = {
    val nextStatus = UInt(2 bits)
    when (jump) {
      switch (status) {
        is (WeaklyNotTaken) { 
          nextStatus := WeaklyTaken
        }
        is (StronglyNotTaken) {
          nextStatus := WeaklyNotTaken
        }
        default {
          nextStatus := StronglyTaken
        }
      }
    } otherwise {
      switch (status) {
        is (WeaklyTaken) {
          nextStatus := WeaklyNotTaken
        }
        is (StronglyTaken) {
          nextStatus := WeaklyTaken
        }
        default {
          nextStatus := StronglyNotTaken
        }
      }
    }
    val t = new TwoBitCounter
    t.status := nextStatus
    t
  }

  def updateWhen(jump : Bool, cond : Bool) = {
    val t = new TwoBitCounter
    when (cond) { t := this.update(jump) }
    .otherwise { t := this }

    t
  }

  def predictTaken = {
    status(1) === True
  }

  def predictNotTaken = {
    status(1) === False
  }

  def set(value : UInt) = {
    status := value
  }
}

object TwoBitCounter {
  def apply(init : UInt) : TwoBitCounter = {
    val t = new TwoBitCounter
    t.status := init
    t
  }

  def update(t : TwoBitCounter, jump : Bool) = {
    t.update(jump)
  }

  // def updateTaken(t : TwoBitCounter) = t.update(True)

  // def updateNotTaken(t : TwoBitCounter) = t.update(False)

  def set(t : TwoBitCounter, value : UInt) = t.set(value)
}