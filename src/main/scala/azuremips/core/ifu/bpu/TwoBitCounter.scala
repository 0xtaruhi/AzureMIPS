package azuremips.core.ifu.bpu

import spinal.core._
import spinal.lib._

object TwoBitCounterStatus extends SpinalEnum {
  val WeaklyTaken, StronglyTaken, WeaklyNotTaken, StronglyNotTaken = newElement()
}

case class TwoBitCounter() extends Bundle {
  import TwoBitCounterStatus._
  val status = Reg(TwoBitCounterStatus())

  def init(initValue : TwoBitCounterStatus.E) = {
    status := initValue
  }

  def update(jump : Bool) = {
    val nextStatus = TwoBitCounterStatus()
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
    status := nextStatus
  }

  def updateWhen(jump : Bool, cond : Bool) = {
    when (cond) { this.update(jump) }
  }

  def predictTaken = {
    status === WeaklyTaken || status === StronglyTaken
  }

  def predictNotTaken = {
    status === WeaklyNotTaken || status === StronglyNotTaken
  }

  def set(value : TwoBitCounterStatus.E) = {
    status := value
  }
}

object TwoBitCounter {
  def apply(init : TwoBitCounterStatus.E) : TwoBitCounter = {
    val t = new TwoBitCounter
    t.init(init)
    t
  }

  def update(t : TwoBitCounter, jump : Bool) = {
    t.update(jump)
  }

  def updateTaken(t : TwoBitCounter) = t.update(True)

  def updateNotTaken(t : TwoBitCounter) = t.update(False)

  def set(t : TwoBitCounter, value : TwoBitCounterStatus.E) = t.set(value)
}