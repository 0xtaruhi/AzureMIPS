// package azuremips.core.exu

// import spinal.core._
// import spinal.core.sim._

// import azuremips.core._

// class TopLevel extends Component {
  
// }

// object InstFetchTest {
//   def main(args: Array[String]): Unit = {
//     SimConfig.withWave.compile(new Test).doSim{ dut => 
//       dut.clockDomain.forkStimulus(10)
//       // dut.io.a #= 0
//       for (i <- 0 until 1000000 by 100) {
//         // dut.clockDomain.waitSampling()
//         // dut.io.ifJmp.jmpEn #= random
//         // println(dut.io.icache.vaddr.toBigInt)
//         dut.clockDomain.waitSampling()
//         dut.io.a #= i
//         println(i, dut.io.b.toBigInt)
//       }
//     }
//   }
// }