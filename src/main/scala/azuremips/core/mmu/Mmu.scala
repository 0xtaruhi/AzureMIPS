// package azuremips.core.mmu

// import spinal.core._
// import spinal.lib._
// import azuremips.core._

// case class Mmu() extends Component {
//   val io = new Bundle {
//     val asid  = in UInt(8 bits) // cp0 entryHi ASID
//     val fetch = master(TranslateAddrReq())
//     val mem   = Vec(master(TranslateAddrReq()), 2)
//   }

//   val tlb = Tlb()
//   tlb.io.asid := io.asid
//   io.fetch  <> tlb.io.trans(0)
//   io.mem(0) <> tlb.io.trans(1)
//   io.mem(1) <> tlb.io.trans(2)
// }

// object GenMmuVerilog {
//   def main(args: Array[String]): Unit = {
//     SpinalVerilog(Mmu())
//   }
// }