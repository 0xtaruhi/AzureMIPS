package azuremips.sim

import java.io.File
import java.io.FileWriter
import scala.io.Source
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import azuremips.core._

class Top extends Component {
  val io = new Bundle {

  }
}

object CoreSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Top) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
    }
  }
}