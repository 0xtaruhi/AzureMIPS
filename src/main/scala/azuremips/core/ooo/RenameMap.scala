package azuremips.core.ooo

import spinal.core._
import spinal.lib._

import azuremips.core._

case class RenamePort(
  config: CoreConfig = CoreConfig()
) extends Bundle with IMasterSlave {
  class OpInfo extends Bundle {
    val arfAddr    = UInt(5 bits)
    val dataInRob  = Bool()
    val dataRobIdx = UInt(config.robIdxWidth bits)
  }
  val robIdx      = UInt(config.robIdxWidth bits)
  val op1         = new OpInfo
  val op2         = new OpInfo
  val wen         = Bool()
  val destArfAddr = UInt(5 bits)
  override def asMaster {
    in (wen, destArfAddr, op1.arfAddr, op2.arfAddr, robIdx)
    out (op1.dataInRob, op1.dataRobIdx, op2.dataInRob, op2.dataRobIdx)
  }
}

case class RenameMap(
  config: CoreConfig = CoreConfig()
) extends Component {
  val decodeWayNum = config.idConfig.decodeWayNum

  val io = new Bundle {
    val renamePorts = Vec(master(RenamePort(config)), decodeWayNum)
  }

  class RenameMapEntry extends Bundle {
    val dataInRob = Bool()
    val dataRobIdx = UInt(config.robIdxWidth bits)
  }

  val renameMap = Vec(Reg(new RenameMapEntry), 32)
  for (entry <- renameMap) {
    entry.dataInRob init(False)
    entry.dataRobIdx init(0)
  }

  for (i <- 0 until decodeWayNum) {
    for (op <- List(io.renamePorts(i).op1, io.renamePorts(i).op2)) {
      def getDataRobInfo(j: Int): (Bool, UInt) = {
        if (j == 0) {
          (renameMap(op.arfAddr).dataInRob, renameMap(op.arfAddr).dataRobIdx)
        } else {
          val matchPrevious = op.arfAddr === io.renamePorts(j - 1).destArfAddr
          (Mux(matchPrevious, True, getDataRobInfo(j - 1)._1),
            Mux(matchPrevious, io.renamePorts(j - 1).robIdx, getDataRobInfo(j - 1)._2))
        }
      }
      op.dataRobIdx := getDataRobInfo(i)._2
      op.dataInRob  := getDataRobInfo(i)._1 && (op.arfAddr =/= 0) // never rename zero register
    }
  }

  // Update Rename Map
  for (renamePort <- io.renamePorts) {
    when (renamePort.wen && renamePort.destArfAddr =/= 0) {
      renameMap(renamePort.destArfAddr).dataInRob := True
      renameMap(renamePort.destArfAddr).dataRobIdx := renamePort.robIdx
    }
  }
}
