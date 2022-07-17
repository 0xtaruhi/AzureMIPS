// package azuremips.core.rtu

// import spinal.core._
// import spinal.lib._
// import azuremips.core.lsu.CommittedSignals
// import azuremips.core.reg.WriteGeneralRegfilePort

// class Retire extends Component {
//   val io = new Bundle {
//     val committedSignals = Vec(in(CommittedSignals()), 2)
//     val writeRegPort = Vec(master(new WriteGeneralRegfilePort), 2)
//   }

//   (io.committedSignals zip io.writeRegPort).foreach{
//     case (commit, general) => {
//       general.wrEn   := commit.wrRegEn
//       general.wrAddr := commit.wrRegAddr
//       general.wrData := commit.wrRegData
//     }
//   }


// }