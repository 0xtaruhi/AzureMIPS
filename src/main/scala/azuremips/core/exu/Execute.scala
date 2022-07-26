package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.idu.ReadRfSignals
import azuremips.core.ExceptionCode._
import azuremips.core.cp0.ExptInfo
import azuremips.core.cache.CReq._
import azuremips.core.reg.{WriteHiloRegfilePort}


case class ExecutedSignals(debug: Boolean = true) extends Bundle {
  val pc        = debug generate UInt(32 bits)
  val wrRegEn   = Bool()
  val wrRegAddr = UInt(5 bits)
  val wrMemEn   = Bool()
  val rdMemEn   = Bool()
  val signExt   = Bool()
  val memSize   = UInt(3 bits)
  val wrMemMask = UInt(4 bits)
  val memVAddr  = UInt(32 bits)
  val wrData    = UInt(32 bits)
}

class SingleExecute(
  advanced : Boolean = true, debug : Boolean = true
  ) extends Component {
  import azuremips.core.ExceptionCode._
  val io = new Bundle {
    val readrfSignals   = in(new ReadRfSignals)
    val executedSignals = out(new ExecutedSignals)
    val exBypass        = out(new BypassPort)
    val exptValid       = out Bool()
    val exptCode        = out UInt(exptCodeWidth bits)
    val writeHilo       = advanced generate(master(new WriteHiloRegfilePort))
    val hiloData        = advanced generate(in UInt(64 bits))
    val readrfPc        = advanced generate(in UInt(32 bits))
    val redirectEn      = advanced generate(out Bool())
    val redirectPc      = advanced generate(out UInt(32 bits))
  }

  val uop    = io.readrfSignals.uop
  val op1    = io.readrfSignals.op1Data
  val op2    = io.readrfSignals.op2Data
  val imm    = io.readrfSignals.imm
  val pc     = io.readrfSignals.pc
  val wrData = U(0, 32 bits)
  io.executedSignals.wrData := wrData

  // Basic Arithmetic Instructions
  switch (uop) {
    is (uOpAdd, uOpAddu) { wrData := op1 + op2 }
    is (uOpSub, uOpSubu) { wrData := op1 - op2 }
    is (uOpSlt)  { wrData := Mux(S(op1) < S(op2), U(1), U(0)).resized }
    is (uOpSltu) { wrData := Mux(op1 < op2, U(1), U(0)).resized       }
    is (uOpAnd)  { wrData := op1 & op2                                }
    is (uOpOr)   { wrData := op1 | op2                                }
    is (uOpXor)  { wrData := op1 ^ op2                                }
    is (uOpLui)  { wrData := imm |<< 16                        }
    is (uOpNor)  { wrData := ~(op1 | op2)                             }
    is (uOpSll)  { wrData := op2 |<< imm(4 downto 0)           }
    is (uOpSllv) { wrData := op2 |<< op1(4 downto 0)                  }
    is (uOpSrav) { wrData := U(S(op2) |>> op1(4 downto 0))            }
    is (uOpSra)  { wrData := U(S(op2) |>> imm(4 downto 0))     }
    is (uOpSrl)  { wrData := op2 |>> imm(4 downto 0)           }
    is (uOpSrlv) { wrData := op2 |>> op1(4 downto 0)                  }
  }

  // Load & Store
  switch (uop) {
    is (uOpSb, uOpSh, uOpSw) {
      io.executedSignals.wrMemEn := True
      io.executedSignals.rdMemEn := False
      wrData := op2
    }
    is (uOpLb, uOpLh, uOpLbu, uOpLhu, uOpLw) {
      io.executedSignals.wrMemEn := False
      io.executedSignals.rdMemEn := True
    }
    default {
      io.executedSignals.wrMemEn := False
      io.executedSignals.rdMemEn := False
    }
  }
  io.executedSignals.wrRegAddr := io.readrfSignals.wrRegAddr
  io.executedSignals.memVAddr  := io.readrfSignals.imm + op1
  io.executedSignals.wrRegEn   := io.readrfSignals.wrRegEn

  val genStrobeInst = new GenStrobe()
  io.executedSignals.wrMemMask := genStrobeInst.io.strobe
  io.executedSignals.memSize   := genStrobeInst.io.size
  io.executedSignals.signExt   := genStrobeInst.io.isSigned
  genStrobeInst.io.addr        := io.executedSignals.memVAddr
  genStrobeInst.io.op          := io.readrfSignals.uop

  // Exceptions
  io.exptValid := False
  io.exptCode  := 0
  switch (uop) {
    is (uOpAdd) {
      when (op1.msb === op2.msb && op1.msb =/= wrData.msb) {
        io.exptValid := True
        io.exptCode  := EXC_OVF
      }
    }
    is (uOpSub) {
      when (op1.msb =/= op2.msb && op1.msb =/= wrData.msb) {
        io.exptValid := True
        io.exptCode  := EXC_OVF
      }
    }
    is (uOpSh) {
      when (io.executedSignals.memVAddr.lsb =/= False) {
        io.exptValid := True
        io.exptCode  := EXC_ADES
      }
    }
    is (uOpSw) {
      when (io.executedSignals.memVAddr(1 downto 0) =/= U"00") {
        io.exptValid := True
        io.exptCode  := EXC_ADES
      }
    }
    is (uOpLh, uOpLhu) {
      when (io.executedSignals.memVAddr.lsb =/= False) {
        io.exptValid := True
        io.exptCode  := EXC_ADEL
      }
    }
    is (uOpLw) {
      when (io.executedSignals.memVAddr(1 downto 0) =/= U"00") {
        io.exptValid := True
        io.exptCode  := EXC_ADEL
      }
    }
    switch (uop) {
      is (uOpSyscall) {
        io.exptValid := True
        io.exptCode  := EXC_SYSCALL
      }
      is (uOpBreak) {
        io.exptValid := True
        io.exptCode  := EXC_BREAK
      }
    }
  }

  // advanced Instructions
  val advancedUop = advanced generate new Area {
    switch (uop) {  
      is (uOpJal, uOpJalr, uOpBgezal, uOpBltzal) {
        wrData := pc + 8
      }
    }

    val shouldJmp = False
    switch (uop) {
      is (uOpBeq) { shouldJmp := (op1 === op2) }
      is (uOpBne) { shouldJmp := (op1 =/= op2) }
      is (uOpBgez, uOpBgezal) { shouldJmp := op1.msb === False }
      is (uOpBgtz) { shouldJmp := (S(op1) > S(0)) }
      is (uOpBlez) { shouldJmp := (S(op1) < S(0)) }
      is (uOpBltz, uOpBltzal) { shouldJmp := op1.msb === True  }
      is (uOpJ, uOpJal, uOpJalr, uOpJr) { shouldJmp := True    }
    }

    val jmpDestPc = UInt(32 bits)
    switch (uop) {
      is (uOpBeq, uOpBne, uOpBgez, uOpBgezal, uOpBgtz, uOpBlez, uOpBltz, uOpBltzal) {
        jmpDestPc := io.readrfSignals.pc + 4 + io.readrfSignals.imm
      }
      is (uOpJ, uOpJal) {
        jmpDestPc := io.readrfSignals.imm
      }
      is (uOpJr, uOpJalr) {
        jmpDestPc := op1
      }
      default {
        jmpDestPc := 0
      }
    }

    when (shouldJmp && jmpDestPc =/= io.readrfPc) {
      io.redirectEn := True
      io.redirectPc := jmpDestPc
    } otherwise {
      io.redirectEn := False
      io.redirectPc := 0
    }

    // Hi/Lo registers
    io.writeHilo.wrHi     := False
    io.writeHilo.wrLo     := False
    io.writeHilo.hiData   := 0
    io.writeHilo.loData   := 0
    switch (uop) {
      is (uOpMfhi) {
        wrData := io.hiloData(63 downto 32)
      }
      is (uOpMflo) {
        wrData := io.hiloData(31 downto 0)
      }
      is (uOpMthi) {
        io.writeHilo.wrHi   := True
        io.writeHilo.hiData := op1
      }
      is (uOpMtlo) {
        io.writeHilo.wrLo   := True
        io.writeHilo.loData := op1
      }
    }
  }

  // bypass
  io.exBypass.wrRegEn   := io.executedSignals.wrRegEn
  io.exBypass.wrRegAddr := io.executedSignals.wrRegAddr
  io.exBypass.wrData    := wrData
  io.exBypass.isLoad    := io.executedSignals.wrMemEn

  // debug
  if (debug) {
    io.executedSignals.pc := io.readrfSignals.pc
  }
}

class Execute(debug : Boolean = true) extends Component {
  val io = new Bundle {
    val readrfSignals   = in Vec(new ReadRfSignals, 2)
    val executedSignals = out Vec(new ExecutedSignals, 2)
    val except          = master(new ExptInfo)
    val readrfPc        = in UInt(32 bits)
    val redirectEn      = out Bool()
    val redirectPc      = out UInt(32 bits)
    val exBypass        = out Vec(new BypassPort, 2)
    val writeHilo       = master(new WriteHiloRegfilePort)
    val hiloData        = in UInt(64 bits)
  }

  val units = Seq(
    new SingleExecute(advanced = true, debug = debug),
    new SingleExecute(advanced = false, debug = debug)
  )

  for (i <- 0 until 2) {
    units(i).io.readrfSignals := io.readrfSignals(i)
    io.executedSignals(i)     := units(i).io.executedSignals
    io.exBypass(i)            := units(i).io.exBypass
  }

  io.writeHilo <> units(0).io.writeHilo
  units(0).io.hiloData := io.hiloData
  units(0).io.readrfPc := io.readrfPc

  io.except.exptValid := units.map(_.io.exptValid).reduce(_ || _)
  io.except.exptCode  := Mux(units(0).io.exptValid, units(0).io.exptCode, units(1).io.exptCode)

  when (io.except.exptValid) {
    io.redirectEn := True
    io.redirectPc := U"32'hbfc00380"
  } otherwise {
    io.redirectEn := units(0).io.redirectEn
    io.redirectPc := units(0).io.redirectPc
  }

  io.except.pc   := 0
  io.except.inBD := False

}

case class GenStrobe() extends Component {
  val io = new Bundle {
    val addr = in UInt(32 bits)
    val op = in(Uops())
    val strobe = out UInt(4 bits)
    val size = out UInt(3 bits)
    val isSigned = out Bool()
  }
  val addr10 = io.addr(1 downto 0)
  io.strobe := U"0000" // load
  io.size := MSIZE4
  io.isSigned := !(io.op === uOpLbu || io.op === uOpLhu) // unsigned => 0
  switch (io.op) {
    is(uOpSb) {
      io.size := MSIZE1
      switch(addr10) {
        is(1) {
          io.strobe := U"0010"
        }
        is(2) {
          io.strobe := U"0100"
        }
        is(3) {
          io.strobe := U"1000"
        }
        default { // 0
          io.strobe := U"0001"
        }
      }
    } // SB
    is(uOpSh) {
      io.size := MSIZE2
      switch (addr10) {
        is(2) {
          io.strobe := U"1100"
        }
        default { // 0
          io.strobe := U"0011"
        }
      }
    } // SH
    is(uOpSw) {
      io.size := MSIZE4
      io.strobe := U"1111"
    } // SW
    is(uOpLb, uOpLbu) {
      io.size := MSIZE1
    }
    is(uOpLh, uOpLhu) {
      io.size := MSIZE2
    }
    default {}
  }
}

object GenExcute {
  def main(args: Array[String]) {
    SpinalVerilog(new Execute)
  }
}