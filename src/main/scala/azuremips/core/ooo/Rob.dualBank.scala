// package azuremips.core.ooo

// import spinal.core._
// import spinal.lib._

// import azuremips.core._

// object RobInstStatus extends SpinalEnum {
//   val inIssueQuene, executing, inCommitQurene = newElement()
// }

// class PushRobInstInfo extends Bundle {
//   val pc  = UInt(32 bits)
//   val op1 = new Bundle {
//     val arfAddr = UInt(5 bits)
//   }
//   val op2 = new Bundle {
//     val arfAddr = UInt(5 bits)
//   }
//   val dest = new Bundle {
//     val wen     = Bool()
//     val arfAddr = UInt(5 bits)
//   }
//   val isBr      = Bool()
// }

// class RobEntry(
//   config: CoreConfig = CoreConfig()
// ) extends Bundle {
//   val pc = UInt(32 bits)
//   class InstInfo extends Bundle {
//     val inUse       = Bool()
//     val instStatus  = RobInstStatus()
//     val op1 = new Bundle {
//       val arfAddr   = UInt(5 bits)
//     }
//     val op2 = new Bundle {
//       val arfAddr   = UInt(5 bits)
//     }
//     val dest = new Bundle {
//       val wen       = Bool()
//       val wdata     = UInt(32 bits)
//       val arfAddr   = UInt(5 bits)
//     }
//     val exception = new Bundle {
//       val en        = Bool()
//       val code      = UInt(2 bits)
//     }
//   }
//   val inst0 = new InstInfo
//   val inst1 = new InstInfo
// }

// case class ReadRobRegDataPort(
//   config: CoreConfig = CoreConfig()
// ) extends Bundle with IMasterSlave {
//   val robIdx = UInt(config.robIdxWidth bits)
//   val data   = UInt(32 bits)
//   override def asMaster = {
//     in(robIdx)
//     out(data)
//   }
// }

// case class Rob(
//   config: CoreConfig = CoreConfig()
// ) extends Component {
//   val robDepth    = config.robDepth
//   val robIdxWidth = config.robIdxWidth
//   val robRowNum   = config.robRowNum

//   val io = new Bundle {
//     val pushEn            = in Bool()
//     val pushInsts         = Vec(in(new PushRobInstInfo), config.idConfig.decodeWayNum)
//     val flush             = in Bool()
//     val tailPtr           = out UInt(log2Up(robDepth) bits)
//     val tailPtr2Rename    = out UInt(log2Up(robDepth) bits)
//     val readRobRegDataPorts = Vec(master(ReadRobRegDataPort(config)), 4)
//   }

//   val rob = Vec(Reg(new RobEntry(config)), robRowNum)
//   val headPtr = Reg(UInt(robIdxWidth bits)) init(0)
//   val tailPtr = Reg(UInt(robIdxWidth bits)) init(0)
//   val tailRowPtr = tailPtr((robIdxWidth - 1) downto 1)
//   val tailColPtr = tailPtr(0)
//   val tailPtr2Rename = UInt(robIdxWidth bits)
//   io.tailPtr := tailPtr
//   io.tailPtr2Rename := tailPtr2Rename
  
//   when (io.flush) {
//     for (entry <- rob) {
//       entry.inst0.inUse := False
//       entry.inst1.inUse := False
//     }
//   }

//   for (entry <- rob) {
//     entry.inst0.dest.wdata init (0)
//     entry.inst1.dest.wdata init (0)
//   }

//   val pushInstsArea = new Area {
//     // Here we only consider 3 decode ways situation
//     if (config.idConfig.decodeWayNum == 3) {
//       tailPtr2Rename := tailPtr + Mux(
//         !tailColPtr && (io.pushInsts(1).isBr) || 
//         tailColPtr && (io.pushInsts(0).isBr || io.pushInsts(2).isBr),
//         4, 3
//       )
//       tailPtr := tailPtr2Rename
//       val pushInstsRobIdx = Vec(UInt(robIdxWidth bits), config.idConfig.decodeWayNum)
//       when (!tailColPtr) { // headPtr at Upper Bank
//         when (io.pushInsts(1).isBr) {
//           pushInstsRobIdx := Vec(tailPtr, tailPtr + 2, tailPtr + 3)
//         } otherwise {
//           pushInstsRobIdx := Vec(tailPtr, tailPtr + 1, tailPtr + 2)
//         }
//       } otherwise { // headPtr at Lower Bank
//         when (io.pushInsts(0).isBr) {
//           pushInstsRobIdx := Vec(tailPtr + 1, tailPtr + 2, tailPtr + 3)
//         } elsewhen (io.pushInsts(2).isBr) {
//           pushInstsRobIdx := Vec(tailPtr, tailPtr + 1, tailPtr + 3)
//         } otherwise {
//           pushInstsRobIdx := Vec(tailPtr, tailPtr + 1, tailPtr + 2)
//         }
//       }

//       (io.pushInsts zip pushInstsRobIdx).map({ case (pushInst, robIdx) =>
//         val robRowIdx = robIdx((robIdxWidth - 1) downto 1)
//         val robColIdx = robIdx(0)
//         val instBank = Mux(robColIdx, rob(robRowIdx).inst1, rob(robRowIdx).inst0)
//         when (robColIdx === False) {
//           rob(robRowIdx).pc := pushInst.pc
//         }// Upper Bank write Pc
//         instBank.inUse        := True
//         instBank.instStatus   := RobInstStatus.inIssueQuene
//         instBank.op1.arfAddr  := pushInst.op1.arfAddr
//         instBank.op2.arfAddr  := pushInst.op2.arfAddr
//         instBank.dest.wen     := pushInst.dest.wen
//         instBank.dest.wdata   := 0
//         instBank.dest.arfAddr := pushInst.dest.arfAddr
//         instBank.exception.en := False
//         instBank.exception.code := 0
//       })
//     }
//   }

//   for (readRobRegDataPort <- io.readRobRegDataPorts) {
//     val rowIdx = readRobRegDataPort.robIdx((robIdxWidth - 1) downto 1)
//     val colIdx = readRobRegDataPort.robIdx(0)
//     val robBank = Mux(colIdx, rob(rowIdx).inst1, rob(rowIdx).inst0)
//     readRobRegDataPort.data := robBank.dest.wdata
//   }
// }
