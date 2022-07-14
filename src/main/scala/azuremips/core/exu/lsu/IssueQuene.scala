package azuremips.core.exu.lsu

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
  val robAddr = UInt(config.robAddrWidth bits)
  val rtOp   = new OpInfo()
  val baseOp = new OpInfo()
  val destOp = new Bundle {
    val wen = Bool()
  }
}

case class InstInfo2IssueQuene(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val valid = Bool()
  val robAddr = UInt(config.robAddrWidth bits)
  val rtOp   = new OpInfo()
  val baseOp = new OpInfo()
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
  issueQueneSize: Int = ?,  // ### how many? ###
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val instsPack = in(Vec(InstInfo2IssueQuene(config), config.idConfig.decodeWayNum))
    val full = out Bool()
    val fuBypass = Vec(in(fuBypassInfo(config)), config.exConfig.fuNum)
    // val inst2Lsu = 
  }

  val quene = Vec(IssueEntry(config) setAsReg, issueQueneSize)
  val queneAddrWidth = log2Up(issueQueneSize)

  val headPtr = Reg(UInt(queneAddrWidth bits)) init(0)
  val tailPtr = Reg(UInt(queneAddrWidth bits)) init(0)
  val diffCycle = Reg(Bool()) init(False)

  val occupiedNum = Mux(diffCycle, issueQueneSize - (headPtr - tailPtr), tailPtr - headPtr)
  // val availNum = issueQueneSize - occupiedNum

  io.full := (availNum < config.idConfig.decodeWayNum)

  val validInstCnt = UInt(log2Up(config.idConfig.decodeWayNum) bits)
  validInstCnt := io.instsPack.map(_.valid.asUInt.resize(validInstCnt.getWidth)).reduce(_ + _)

  val update = new Area {
    val nextHeadPtr = UInt(log2Up(issueQueneSize) bits)
    val nextTailPtr = UInt(log2Up(issueQueneSize) bits)

    if (isPow2(issueQueneSize)) {
      // TODO: or not to do
    } else {
      when(!io.full) {
        
      } otherwise { nextTailPtr := tailPtr }
    }
  }

}