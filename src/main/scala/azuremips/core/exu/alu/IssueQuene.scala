package azuremips.core.exu.alu

import spinal.core._
import spinal.lib._

import azuremips.core._

class OpInfo(config: CoreConfig = CoreConfig()) extends Bundle {
  val addrWidth = config.robAddrWidth
  val ready = Bool()
  val addr  = UInt(addrWidth bits)
  val payload = UInt(32 bits)
}

case class IssueEntry(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val inUse  = Bool()
  val issued = Bool()
  val robAddr = UInt(config.robAddrWidth bits)
  val op1 = new OpInfo()
  val op2 = new OpInfo()
  val destOp = new Bundle {
    val wen = Bool()
  }
}

case class InstInfo2IssueQuene(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val robAddr = UInt(config.robAddrWidth bits)
  val op1 = new OpInfo()
  val op2 = new OpInfo()
  val destOp = new Bundle {
    val wen = Bool()
  }
}

case class fuBypassInfo(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val robAddr = UInt(config.robAddrWidth bits)
  val wen     = Bool()
  val payload = UInt(32 bits)
}

class IssueQuene(
  issueQueneSize: Int = 12,
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val newInst        = in(InstInfo2IssueQuene(config))
    // val wakeUp         = Vec(in(UInt(config.robAddrWidth bits)), config.exConfig.fuNum)
    val issueReq       = Vec(master(InstInfo2Arbiter(issueQueneSize, config)), issueQueneSize)
    val issueOk        = in Bool()
    val issueOkInstIdx = in(UInt(log2Up(issueQueneSize) bits))
    val fuBypass       = Vec(in(fuBypassInfo(config)), config.exConfig.fuNum)
  }
  val quene = Vec(IssueEntry(config) setAsReg, issueQueneSize)

  val newInstArea = new Area {
    val newIssueEntry = IssueEntry(config)
    newIssueEntry.inUse  := True
    newIssueEntry.issued := False
    newIssueEntry.robAddr := io.newInst.robAddr
    newIssueEntry.op1 <> io.newInst.op1
    newIssueEntry.op2 <> io.newInst.op2
    newIssueEntry.destOp.wen := io.newInst.destOp.wen

    // val freeEntryIdx = FindFreeEntryIdx(quene, foldWidth=4)
    val freeEntryIdx = FindFreeEntryIdx(quene, foldWidth=4)
    val hasFreeEntry = !quene.map(_.inUse).andR
    when(hasFreeEntry) {
      quene(freeEntryIdx) := newIssueEntry
    }
  }

  val selectInst = quene(io.issueOkInstIdx)
  val selectArea = new Area {
    (io.issueReq zip quene).foreach {
      case (request, entry) => {
        val prevIssueInstHitOp1 = RegNext(selectInst.destOp.wen) && 
                                  RegNext(selectInst.robAddr) === entry.op1.addr
        val prevIssueInstHitOp2 = RegNext(selectInst.destOp.wen) && 
                                  RegNext(selectInst.robAddr) === entry.op2.addr

        when ((entry.op1.ready || prevIssueInstHitOp1) &&
              (entry.op2.ready || prevIssueInstHitOp2)) {
          request.ready   := True
          request.robAddr := entry.robAddr
        } otherwise {
          request.ready   := False
          request.robAddr := 0
        }
      }
    }

    for (i <- 0 to issueQueneSize-1) {
      io.issueReq(i).issueQueneIdx := U(i)
    }
  }

  val wakeUpArea = new Area {
    for (entry <- quene) {
      for (fuBypass <- io.fuBypass) {
        when (fuBypass.wen) {
          when (fuBypass.robAddr === entry.op1.addr) {
            entry.op1.payload := fuBypass.payload
            entry.op1.ready   := True
          }
          when (fuBypass.robAddr === entry.op2.addr) {
            entry.op2.payload := fuBypass.payload
            entry.op2.ready   := True
          }
        }
      }
    } 
  }

}

object FindFreeEntryIdx {
  def apply(busyTable: Vec[IssueEntry], foldWidth: Int = 2): UInt = {
    PriorityMux(for (i <- 0 until busyTable.size) yield {
      (busyTable(i).inUse && !busyTable(i).issued, U(i))
    }).resized
  }
}

object genIssueQueneVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new IssueQuene(issueQueneSize = 12))
  }
}