// package azuremips.core.ifu

// import spinal.core._
// import spinal.core.sim._

// import azuremips.core._

// class TopLevel extends Component {
  
// }

// object InstFetchTest {
//   def main(args: Array[String]): Unit = {
//     SimConfig.withWave.compile(new InstFetch).doSim{ dut => 
//       dut.clockDomain.forkStimulus(10)

//       dut.io.ifJmp.jmpEn #= false
//       dut.io.ifJmp.jmpDest #= 0x0
//       dut.io.fetchBufFull #= false

//       for (i <- 0 until 10) {
//         dut.clockDomain.waitSampling()
//         dut.io.ifJmp.jmpEn #= random
//         println(dut.io.icache.vaddr.toBigInt)
//       }
//     }
//   }
// }