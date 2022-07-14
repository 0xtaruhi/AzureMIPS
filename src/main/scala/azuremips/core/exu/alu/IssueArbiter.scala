package azuremips.core.exu.alu

import spinal.core._
import spinal.lib._

import azuremips.core._

case class InstInfo2Arbiter(
  issueQueneSize: Int = 12,
  config: CoreConfig = CoreConfig()
) extends Bundle with IMasterSlave {
  val robAddr = UInt(config.robAddrWidth bits)
  val issueQueneIdx = UInt(log2Up(issueQueneSize) bits)
  val ready = Bool()
  override def asMaster {
    out(robAddr)
    out(issueQueneIdx)
    out(ready)
  }
}

class IssueArbiter(
  issueQueneSize: Int = 16,
  config: CoreConfig = CoreConfig()
) extends Component {
  val io = new Bundle {
    val instsInfo = Vec(slave(InstInfo2Arbiter(issueQueneSize, config)), issueQueneSize)
    val issueOkInst = master(InstInfo2Arbiter(issueQueneSize, config))
  }

  def _ageCmpRecur(
    _instsInfo: Vec[InstInfo2Arbiter]
    ): Vec[InstInfo2Arbiter] = {
    val vecSize = _instsInfo.size
    vecSize match {
      case 1 => _instsInfo
      case _ => {
        val _vec = Vec(
          InstInfo2Arbiter(issueQueneSize, config),
          ((vecSize + 1) / 2).toInt
        )
        for (i <- 0 until _vec.size) {
          if (2 * i < vecSize) {
            val instInfoL = _instsInfo(i * 2 + 0)
            val instInfoR = _instsInfo(i * 2 + 1)
            val lAgeLtR = AgeWithPosCmp(instInfoL.robAddr, instInfoR.robAddr)
            val allReadyInfo = Mux(lAgeLtR, instInfoL, instInfoR)
            switch (instInfoL.ready ## instInfoR.ready) {
              is (B"11") {
                _vec(i).robAddr := allReadyInfo.robAddr
                _vec(i).issueQueneIdx := allReadyInfo.issueQueneIdx
              }
              is (B"10") {
                _vec(i).robAddr := instInfoL.robAddr
                _vec(i).issueQueneIdx := instInfoL.issueQueneIdx
              }
              is (B"01") {
                _vec(i).robAddr := instInfoR.robAddr
                _vec(i).issueQueneIdx := instInfoR.issueQueneIdx
              }
              default {
                _vec(i).robAddr := allReadyInfo.robAddr
                _vec(i).issueQueneIdx := allReadyInfo.issueQueneIdx
              }
            }
            _vec(i).ready := instInfoL.ready || instInfoR.ready
          } else {
            _vec(i) := _instsInfo(i * 2)
          }
        }
        _ageCmpRecur(_vec)
      }
    }
  }

  io.issueOkInst := _ageCmpRecur(io.instsInfo)(0)

}

object AgeWithPosCmp {
  def apply(a: UInt, b: UInt) = {
    val aPos = a.msb
    val bPos = b.msb
    val aAddr = a(a.high - 1 downto 0)
    val bAddr = b(b.high - 1 downto 0)
    val addrLt = aAddr < bAddr
    (aPos === bPos) ? addrLt | !addrLt
  }
}

object IssueArbiter {
  def apply(instsInfo: Vec[InstInfo2Arbiter]): UInt = {
    val issueArbiter = new IssueArbiter(instsInfo.size)
    issueArbiter.io.instsInfo := instsInfo
    issueArbiter.io.issueOkInst.issueQueneIdx
  }
}

object genIssueArbiterVerilog {
  def main(args: Array[String]) {
    SpinalSystemVerilog(new IssueArbiter(16))
  }
}