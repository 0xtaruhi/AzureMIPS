package azuremips.core.cache

import spinal.core._
import spinal.lib._

import azuremips.core._

case class PLRU (config: ICacheConfig = ICacheConfig()) extends Component {
    val io = new Bundle {
        val plruport = slave(PlruPort(config))
    }

    if (config.wayNum == 1) {
        io.plruport.victim_idx := io.plruport.selected_idx
        io.plruport.select_nxt := U(0)
    } else if (config.wayNum == 2) {
        io.plruport.victim_idx := U(0)
        io.plruport.select_nxt := Mux(io.plruport.is_hit, io.plruport.selected_idx.resized, ~io.plruport.select)

        when (io.plruport.valids.andR) {
            io.plruport.victim_idx := ~(io.plruport.select.resized)
        } .otherwise {
            for (i <- 0 until config.wayNum) {
                when (io.plruport.valids(i) === False) {
                    io.plruport.victim_idx := U(i).resized
                }
            }
        }
    } else if (config.wayNum == 4) {
        io.plruport.victim_idx := U(0)
        when (io.plruport.valids.andR) {
            io.plruport.victim_idx(1) := ~ io.plruport.select(2)
            when (~ io.plruport.select(2)) {
                io.plruport.victim_idx(0) := ~ io.plruport.select(0)
            }.otherwise {
                io.plruport.victim_idx(0) := ~ io.plruport.select(1)
            }
        } .otherwise {
            for (i <- 0 until config.wayNum) {
                when (io.plruport.valids(i) === False) {
                    io.plruport.victim_idx := U(i).resized
                }
            }
        }

        io.plruport.select_nxt := io.plruport.select
        val idx_tmp = Mux(io.plruport.is_hit, io.plruport.selected_idx, io.plruport.victim_idx)
        io.plruport.select_nxt(2) := idx_tmp(1)
        when (idx_tmp(1) === True) {
            io.plruport.select_nxt(0) := idx_tmp(0)
        }.otherwise {
            io.plruport.select_nxt(1) := idx_tmp(0)
        }
    } else {
        io.plruport.select_nxt := io.plruport.select
        io.plruport.victim_idx := ~ io.plruport.selected_idx
    }
    
}

case class PlruPort(config: ICacheConfig = ICacheConfig()) extends Bundle with IMasterSlave {
    val select = UInt(config.selectWidth bits)
    val selected_idx = UInt(config.idxWidth bits)
    val valids = UInt(config.validRamWordWidth bits)
    val is_hit = Bool()
    val victim_idx = UInt(config.idxWidth bits)
    val select_nxt = UInt(config.selectWidth bits)
    
    override def asMaster() {
        in (victim_idx, select_nxt)
        out (select, selected_idx, valids, is_hit)
    }
}

object PLRU {
    def main(args: Array[String]) {
        SpinalVerilog(PLRU(ICacheConfig()))
    }
}