package azuremips.core.ooo

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.ExceptionCode._

case class PushRobInstInfo(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val validInst   = Bool()
  val pc          = UInt(32 bits)
  val destWen     = Bool()
  val destArfAddr = Bool()
  val isBr        = Bool()
}

case class RobEntry(
  config: CoreConfig = CoreConfig()
) extends Bundle {
  val valid       = Bool()
  val pc          = UInt(32 bits)
  val destWen     = Bool()
  val destArfAddr = Bool()
  val excuted     = Bool()
  val destData    = UInt(32 bits)
  val isBr        = Bool()
  val exptValid   = Bool()
  val exptCode    = UInt(exptCodeWidth bits)
  val brEn        = Bool()
}

case class DestDataFromRob(
  config: CoreConfig = CoreConfig()
) extends Bundle with IMasterSlave {
  val robAddr = UInt((log2Up(config.robDepth) + 1) bits)
  val data    = UInt(32 bits)

  override def asMaster(): Unit = {
    out(robAddr)
    in(data)
  }
}

case class Rob(
  config: CoreConfig = CoreConfig()
) extends Component {
  val decodeWayNum = config.idConfig.decodeWayNum
  val robDepth = config.robDepth
  require(isPow2(robDepth))
  val robAddrWidth = log2Up(robDepth) + 1 // msb is for position bit
  val io = new Bundle {
    val decodeInstsValid = in Bool()
    val decode2RobInsts  = Vec(in(PushRobInstInfo(config)), decodeWayNum)
    val destDataPorts    = Vec(slave(DestDataFromRob(config)), decodeWayNum * 2)
    val expt             = master(cp0.ExptInfo(config))
    val flushPipeline    = out Bool()
    val tailPtr          = out UInt(robAddrWidth bits)
  }

  val headPtr = Reg(UInt(robAddrWidth bits)) init 0
  val tailPtr = Reg(UInt(robAddrWidth bits)) init 0
  val rob     = Vec(Reg(RobEntry(config)), robDepth)
  io.tailPtr := tailPtr

  val headPtrWithoutPos = headPtr(headPtr.high - 1 downto 0)
  val tailPtrWithoutPos = tailPtr(tailPtr.high - 1 downto 0)
  val headPtrPos = headPtr.msb
  val tailPtrPos = tailPtr.msb

  val full = (headPtr - tailPtr) < decodeWayNum

  for (destDataPort <- io.destDataPorts) {
    destDataPort.data.setAsReg.init(0)
    val addr = destDataPort.robAddr(destDataPort.robAddr.high - 1 downto 0)
    destDataPort.data := rob(addr).destData
  }

  val pushInstsArea = new Area {
    when (io.decodeInstsValid && !full) {
      for (i <- 0 until io.decode2RobInsts.size) {
        val inst = io.decode2RobInsts(i)
        val entry = rob(tailPtrWithoutPos + i)
        entry.valid := True
        entry.pc    := inst.pc
        entry.brEn  := False
        entry.destData := U(0, 32 bits)
        when (inst.validInst) {
          entry.destWen     := inst.destWen
          entry.destArfAddr := inst.destArfAddr
          entry.isBr        := inst.isBr
          entry.exptValid   := False
          entry.exptCode    := U(0, 4 bits)
        } otherwise {
          entry.destWen     := False
          entry.excuted     := True
          entry.isBr        := False
          entry.exptValid   := True
          entry.exptCode    := EXC_RESERVED
        }
      }
      tailPtr := tailPtr + decodeWayNum
    }
  }

  val retireArea = new Area {
    val retireInstNum = config.oooConfig.retireInstNum
    val retireInsts = for (i <- 0 until retireInstNum) yield {
      rob(headPtrWithoutPos + i)
    }
    /* headPtrMoveStep is equal to the number of retired instructions 
      in current cycle.
    */
    val headPtrMoveStep = UInt(log2Up(retireInstNum) bits)
    headPtrMoveStep := retireInstNum
    when (retireInsts.last.isBr) {
      headPtrMoveStep := retireInstNum - 1
    }
    // Exception
    io.expt.exptValid := False
    io.expt.exptCode  := 0
    io.expt.inBD      := False
    io.expt.pc        := 0

    io.flushPipeline  := False

    for (i <- (0 until retireInstNum).reverse) {
      when (!retireInsts(i).excuted) {
        headPtrMoveStep := i
      } otherwise {
        when (retireInsts(i).exptValid) {
          io.expt.exptValid := True
          io.expt.exptCode  := retireInsts(i).exptCode
          io.expt.pc        := retireInsts(i).pc
          if (i > 0) {
            io.expt.inBD := retireInsts(i-1).brEn
          }
        }
        // when (retireInsts(i).isBr) {

        // }
      } 
    }

    for (i <- 0 until retireInstNum) {
      when (i < headPtrMoveStep) {
        rob(headPtrWithoutPos + i).valid := False
      }
    }

    headPtr := headPtr + headPtrMoveStep
  }
}

object GenRobVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Rob)
  }
}