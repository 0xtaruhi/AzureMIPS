// package azuremips.core.exu

// import spinal.core._
// import spinal.lib._

// import azuremips.core._

// class IssueEntry(
//   config: CoreConfig = CoreConfig()
// ) extends Bundle {
//   val inUse  = Bool()
//   val ready  = Bool()
//   val issued = Bool()
//   class OpInfo extends Bundle {
//     val addrWidth = max(log2Up(config.robDepth), 5)
//     val valid = Bool()
//     val ready = Bool()
//     val inRob = Bool()
//     val addr  = UInt(addrWidth bits)
//     val payload = UInt(32 bits)
//   }
//   val op1 = new OpInfo()
//   val op2 = new OpInfo()
// }

// class IssueQuene(
//   issueWidth: Int = 12,
//   config: CoreConfig = CoreConfig()
// ) extends Component {
//   val io = new Bundle {
//     val wakeUp = Vec(in(UInt(log2Up(config.robDepth) bits)), config.exConfig.fuNum)
//   }
//   val quene = Vec(new IssueEntry(config), issueWidth)
//   for (entry <- quene) {
//     val issueReq = Bool()
//     entry.ready.setAsReg() init (False)

//   }
// }