package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core._

case class Random(config: DCacheConfig = DCacheConfig()) extends Component {
    val io = new Bundle {
        val ramdomport = slave(RandomPort(config))
    }

    val counter = RegInit(U(0, config.idxWidth bits))
    counter := counter + 1

    when (io.randomport.valids.andR) {
        io.randomport.victim_idx := counter
    } .otherwise {
        for (i <- 0 until config.wayNum) {
            when (io.randomport.valids(i) === False) {
                io.randomport.victim_idx := U(i).resized
            }
        }
    }
}

case class RandomPort(config: DCacheConfig = DCacheConfig()) extends Bundle with IMasterSlave {
  val valids = UInt(config.validRamWordWidth bits)
  val victim_idx = UInt(config.idxWidth bits)

  override def asMaster() {
    in (victim_idx)
    out (valids)
  }
}

object Random {
    def main(args: Array[String]) {
        SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    )
    .generateVerilog(new Random)
    }
}