package azuremips.core.ifu

import java.io.File
import java.io.FileWriter
import scala.io.Source
import spinal.core._
import spinal.lib._
import spinal.core.sim._

import azuremips.core._

class TopLevel(config: CoreConfig = CoreConfig()) extends Component {
  val io = new Bundle {
    val fetchBufFull = in Bool()
    val ifJmp        = in(ifu.JumpInfo(config))
    val icache       = master(ifu.IF2ICache(config))
    val signals      = out(Vec(idu.InstSignals(config), config.idConfig.decodeWayNum))
  }

  val instFetch = new ifu.InstFetch(config)
  val fetchBuffer = new ifu.FetchBuffer(config)
  val decoders = for (i <- 0 until config.idConfig.decodeWayNum) yield { new idu.Decoder(config) }

  // instfetch -> fetchbuffer
  instFetch.io.instsPack <> fetchBuffer.io.instsPack
  instFetch.io.ifJmp := io.ifJmp
  instFetch.io.fetchBufFull := fetchBuffer.io.full
  instFetch.io.icache <> io.icache

  // fetchbuffer -> decoder
  (fetchBuffer.io.insts2Decode zip decoders).foreach {
    case (fetchInst, decoder) =>
      decoder.io.inst := RegNext(fetchInst)
  }

  (io.signals zip decoders).foreach {
    case (signal, decoder) =>
      signal := decoder.io.signals
  }

}

object CoreTest {
  def main(args: Array[String]): Unit = {
    // SimConfig.withWave.compile(new Test).doSim{ dut => 
    //   dut.clockDomain.forkStimulus(10)
    // }
    val text = Source.fromFile("/home/taruhi/Project/AzureMIPS/src/main/hex/test.onlycode.txt").getLines
    SimConfig.withWave.compile(new TopLevel).doSim{ dut =>
      dut.clockDomain.forkStimulus(10)
      // dut.io.fetchBufFull #= false
      dut.io.ifJmp.jmpEn #= false
      dut.io.icache.instValids.foreach(_ #= true)
      dut.io.icache.hit #= true
      for (i <- 0 until 40) {
        dut.io.icache.insts.foreach(_ #= BigInt(text.next, 16))
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}