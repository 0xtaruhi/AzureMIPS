package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.idu.ReadRfSignals
import azuremips.core.ExceptionCode._
import azuremips.core.cp0.ExptInfo
import azuremips.core.cache.CReq._
import azuremips.core.reg.WriteHiloRegfilePort
import azuremips.core.cp0.{Cp0ReadPort, Cp0WritePort}

case class ExecutedSignals() extends Bundle {
  val pc        = UInt(32 bits)
  val wrRegEn   = Bool()
  val wrCp0En   = Bool()
  val wrRegAddr = UInt(5 bits)
  val cp0Addr   = UInt(5 bits)
  val cp0Sel    = UInt(3 bits)
  val wrMemEn   = Bool()
  val rdMemEn   = Bool()
  val rdCp0En   = Bool()
  val signExt   = Bool()
  val memSize   = UInt(3 bits)
  val wrMemMask = UInt(4 bits)
  val memVAddr  = UInt(32 bits)
  val wrData    = UInt(32 bits)
  val except    = ExptInfo()
  val isBr      = Bool()

  def nopExecutedSignals() = {
    val s = ExecutedSignals()
    s.pc        := 0
    s.wrRegEn   := False
    s.wrRegAddr := 0
    s.cp0Addr   := 0
    s.cp0Sel    := 0
    s.wrMemEn   := False
    s.rdMemEn   := False
    s.signExt   := False
    s.memSize   := 0
    s.wrMemMask := 0
    s.memVAddr  := 0
    s.wrData    := 0
    s.except    := ExptInfo().emptyExptInfo
    s.isBr      := False
    s.wrCp0En   := False
    s.rdCp0En   := False
    s
  }
}

class SingleExecute(
  advanced : Boolean = true
  ) extends Component {
  val io = new Bundle {
    val readrfSignals   = in(new ReadRfSignals)
    val executedSignals = out(new ExecutedSignals)
    val exBypass        = out(new BypassPort)
    val writeHilo       = master(new WriteHiloRegfilePort)
    val hiloData        = in UInt(64 bits)
    val multicycleInfo  = out(new MulticycleInfo())
    val redirectEn      = out Bool()
    val redirectPc      = out UInt(32 bits)
    val readrfPc        = advanced generate(in UInt(32 bits))
  }

  val uop    = io.readrfSignals.uop
  val op1    = io.readrfSignals.op1Data
  val op2    = io.readrfSignals.op2Data
  val imm    = io.readrfSignals.imm
  val pc     = io.readrfSignals.pc
  val wrData = U(0, 32 bits)
  io.executedSignals.wrData := wrData
  io.executedSignals.cp0Addr := io.readrfSignals.cp0Addr

  // Basic Arithmetic Instructions
  switch (uop) {
    is (uOpAdd, uOpAddu) { wrData := op1 + op2 }
    is (uOpSub, uOpSubu) { wrData := op1 - op2 }
    is (uOpSlt)  { wrData := Mux(S(op1) < S(op2), U(1), U(0)).resized }
    is (uOpSltu) { wrData := Mux(op1 < op2, U(1), U(0)).resized       }
    is (uOpAnd)  { wrData := op1 & op2                                }
    is (uOpOr)   { wrData := op1 | op2                                }
    is (uOpXor)  { wrData := op1 ^ op2                                }
    is (uOpLui)  { wrData := imm |<< 16                               }
    is (uOpNor)  { wrData := ~(op1 | op2)                             }
    is (uOpSll)  { wrData := op2 |<< imm(4 downto 0)                  }
    is (uOpSllv) { wrData := op2 |<< op1(4 downto 0)                  }
    is (uOpSrav) { wrData := U(S(op2) |>> op1(4 downto 0))            }
    is (uOpSra)  { wrData := U(S(op2) |>> imm(4 downto 0))            }
    is (uOpSrl)  { wrData := op2 |>> imm(4 downto 0)                  }
    is (uOpSrlv) { wrData := op2 |>> op1(4 downto 0)                  }
  }

  io.redirectEn := False
  io.redirectPc := 0
  // Load & Store
  io.executedSignals.wrRegAddr := io.readrfSignals.wrRegAddr
  io.executedSignals.memVAddr  := io.readrfSignals.imm + op1
  io.executedSignals.wrRegEn   := io.readrfSignals.wrRegEn
  io.executedSignals.isBr      := io.readrfSignals.isBr
  io.executedSignals.pc        := io.readrfSignals.pc

  val genStrobeInst = new GenStrobe()
  io.executedSignals.wrMemMask := genStrobeInst.io.strobe
  io.executedSignals.memSize   := genStrobeInst.io.size
  io.executedSignals.signExt   := genStrobeInst.io.isSigned
  genStrobeInst.io.addr        := io.executedSignals.memVAddr
  genStrobeInst.io.op          := io.readrfSignals.uop
  genStrobeInst.io.raw_data    := op2

  switch (uop) {
    is (uOpSb, uOpSh, uOpSw) {
      io.executedSignals.wrMemEn := True
      io.executedSignals.rdMemEn := False
      wrData := genStrobeInst.io.data_o
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

  val exptValid = False
  val exptCode  = U(0)
  val advancedUop = advanced generate new Area {
    //------------BRANCH INSTRUCTIONS------------------
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
      is (uOpBlez) { shouldJmp := (S(op1) <= S(0)) }
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
        when (op1(1 downto 0) =/= U"00") {
          exptValid := True
          exptCode  := EXC_ADEL_FI
          io.executedSignals.memVAddr := op1
          // io.executedSignals.wrRegEn  := False
        }
      }
      default {
        jmpDestPc := 0
      }
    }
    when (shouldJmp && jmpDestPc =/= io.readrfPc) {
      io.redirectEn := True
      io.redirectPc := jmpDestPc
    }
  }
  // Exceptions
  io.executedSignals.except.exptValid   := exptValid
  io.executedSignals.except.exptCode    := exptCode
  io.executedSignals.except.eret        := False
  switch (uop) {
    is (uOpAdd) {
      when (op1.msb === op2.msb && op1.msb =/= wrData.msb) {
        exptValid := True
        exptCode  := EXC_OVF
        io.executedSignals.wrRegEn := False
      }
    }
    is (uOpSub) {
      when (op1.msb =/= op2.msb && op1.msb =/= wrData.msb) {
        exptValid := True
        exptCode  := EXC_OVF
        io.executedSignals.wrRegEn := False
      }
    }
    is (uOpSh) {
      when (io.executedSignals.memVAddr.lsb =/= False) {
        exptValid := True
        exptCode  := EXC_ADES
        io.executedSignals.wrMemEn := False
      }
    }
    is (uOpSw) {
      when (io.executedSignals.memVAddr(1 downto 0) =/= U"00") {
        exptValid := True
        exptCode  := EXC_ADES
        io.executedSignals.wrMemEn := False
      }
    }
    is (uOpLh, uOpLhu) {
      when (io.executedSignals.memVAddr.lsb =/= False) {
        exptValid := True
        exptCode  := EXC_ADEL
        io.executedSignals.wrRegEn := False
        io.executedSignals.rdMemEn := False
      }
    }
    is (uOpLw) {
      when (io.executedSignals.memVAddr(1 downto 0) =/= U"00") {
        exptValid := True
        exptCode  := EXC_ADEL
        io.executedSignals.wrRegEn := False
        io.executedSignals.rdMemEn := False
      }
    }
  }
  when (!io.readrfSignals.validInst) {
    exptValid := True
    exptCode  := EXC_RESERVED
  }
  //------------HI/LO INSTRUCTIONS------------------
  io.writeHilo.wrHi     := False
  io.writeHilo.wrLo     := False
  io.writeHilo.hiData   := 0
  io.writeHilo.loData   := 0
  io.multicycleInfo.multiplyValid     := False
  io.multicycleInfo.divValid          := False
  io.multicycleInfo.isSigned          := False
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
    is (uOpMult) {
      // val multResult = U(S(op1) * S(op2))
      io.multicycleInfo.multiplyValid  := True
      io.multicycleInfo.isSigned := True
      io.writeHilo.wrHi   := True
      io.writeHilo.wrLo   := True
    }
    is (uOpMultu) {
      // val multuResult = op1 * op2
      io.multicycleInfo.multiplyValid  := True
      io.writeHilo.wrHi   := True
      io.writeHilo.wrLo   := True
    }
    is (uOpDiv) {
      io.multicycleInfo.divValid  := True
      io.multicycleInfo.isSigned  := True
      io.writeHilo.wrHi   := True
      // io.writeHilo.hiData := U(S(op1) % S(op2))
      io.writeHilo.wrLo   := True
      // io.writeHilo.loData := U(S(op1) / S(op2))
    }
    is (uOpDivu) {
      io.writeHilo.wrHi   := True
      io.multicycleInfo.divValid := True
      // io.writeHilo.hiData := op1 % op2
      io.writeHilo.wrLo   := True
      // io.writeHilo.loData := op1 / op2
    }
  }
  //-------------PRIVILEGE INSTRUCTIONS------------------
  switch (uop) {
    is (uOpBreak) {
      exptValid := True
      exptCode  := EXC_BREAK
    }
    is (uOpSyscall) {
      exptValid := True
      exptCode  := EXC_SYSCALL
    }
    is (uOpEret) {
      exptValid := True
      exptCode  := EXC_ERET
      io.executedSignals.cp0Addr     := 14 // EPC register
      io.executedSignals.except.eret := True
      io.redirectEn := True
      io.redirectPc := io.readrfSignals.pc
    }
  }
  io.executedSignals.cp0Sel := io.readrfSignals.imm(2 downto 0)
  io.executedSignals.wrCp0En := False
  io.executedSignals.rdCp0En := False
  switch (uop) {
    is (uOpMfc0) {
      wrData := 0
      io.executedSignals.rdCp0En := True
    }
    is (uOpMtc0) {
      wrData        := op1
      io.executedSignals.wrCp0En := True
      io.redirectEn := True
      io.redirectPc := io.readrfSignals.pc + 4
    }
  }

  when (exptValid) {
    io.redirectEn := True
    io.redirectPc := io.readrfSignals.pc
  } // TODO: when EXL=1, should not redirect


  // bypass
  io.exBypass.wrRegEn   := io.executedSignals.wrRegEn
  io.exBypass.wrRegAddr := io.executedSignals.wrRegAddr
  io.exBypass.wrData    := wrData
  io.exBypass.isLoad    := io.executedSignals.rdMemEn || io.executedSignals.rdCp0En

}

class Execute(debug : Boolean = true) extends Component {
  val io = new Bundle {
    val readrfSignals   = in Vec(new ReadRfSignals, 2)
    val executedSignals = out Vec(new ExecutedSignals, 2)
    val readrfPc        = in UInt(32 bits)
    val redirectEn      = out Bool()
    val redirectPc      = out UInt(32 bits)
    val exBypass        = out Vec(new BypassPort, 2)
    val writeHilo       = master(new WriteHiloRegfilePort)
    val hiloData        = in UInt(64 bits)
    val multiCycleStall = out Bool()
  }

  val units = Seq(
    new SingleExecute(advanced = true),
    new SingleExecute(advanced = false)
  )

  for (i <- 0 until 2) {
    units(i).io.readrfSignals := io.readrfSignals(i)
    io.executedSignals(i)     := units(i).io.executedSignals
    io.exBypass(i)            := units(i).io.exBypass
    units(i).io.hiloData         := io.hiloData
  }
  units(0).io.readrfPc := io.readrfPc

  // multicycle insts
  val hiDataOfMfMt    = units.map(_.io.writeHilo.hiData).reduce(_ | _)
  val loDataOfMfMt    = units.map(_.io.writeHilo.loData).reduce(_ | _)

  val multUnit = new Multiplier
  multUnit.io.valid    := units.map(_.io.multicycleInfo.multiplyValid).reduce(_ || _)
  multUnit.io.isSigned := units.map(_.io.multicycleInfo.isSigned).reduce(_ || _)
  multUnit.io.a        := Mux(io.readrfSignals(0).multiCycle, io.readrfSignals(0).op1Data, io.readrfSignals(1).op1Data)
  multUnit.io.b        := Mux(io.readrfSignals(0).multiCycle, io.readrfSignals(0).op2Data, io.readrfSignals(1).op2Data)
  // multUnit.io.flush    := io.multiCycleFlush
  val divUnit = new Divider
  divUnit.io.valid     := units.map(_.io.multicycleInfo.divValid).reduce(_ || _)
  divUnit.io.isSigned  := units.map(_.io.multicycleInfo.isSigned).reduce(_ || _)
  divUnit.io.a        := Mux(io.readrfSignals(0).multiCycle, io.readrfSignals(0).op1Data, io.readrfSignals(1).op1Data)
  divUnit.io.b        := Mux(io.readrfSignals(0).multiCycle, io.readrfSignals(0).op2Data, io.readrfSignals(1).op2Data)
  // divUnit.io.flush    := io.multiCycleFlush
  val multiCycleStall = ((units.map(_.io.multicycleInfo.multiplyValid).reduce(_ || _) && !multUnit.io.done) || 
                          (units.map(_.io.multicycleInfo.divValid).reduce(_ || _) && !divUnit.io.done))
  // data output switch
  switch(U(1 -> multUnit.io.done, 0 -> divUnit.io.done)) {
    is(U"10") {
      io.writeHilo.hiData := multUnit.io.res(63 downto 32)
      io.writeHilo.loData := multUnit.io.res(31 downto 0)
    }
    is(U"01") {
      io.writeHilo.hiData := divUnit.io.res(63 downto 32)
      io.writeHilo.loData := divUnit.io.res(31 downto 0)
    }
    default {
      io.writeHilo.hiData := hiDataOfMfMt
      io.writeHilo.loData := loDataOfMfMt
    }
  }
  
  io.writeHilo.wrHi   := units.map(_.io.writeHilo.wrHi).reduce(_ || _) && !multiCycleStall
  io.writeHilo.wrLo   := units.map(_.io.writeHilo.wrLo).reduce(_ || _) && !multiCycleStall
  io.multiCycleStall  := multiCycleStall
  // multicycle end

  // redirect
  when (units(0).io.executedSignals.except.exptValid && 
        !units(0).io.executedSignals.isBr) {
    io.executedSignals(1) := ExecutedSignals().nopExecutedSignals
    io.writeHilo.wrHi := False
    io.writeHilo.wrLo := False
  }
  io.redirectEn := units.map(_.io.redirectEn).reduce(_ || _)
  io.redirectPc := Mux(units(0).io.redirectEn, units(0).io.redirectPc, units(1).io.redirectPc)
}

case class GenStrobe() extends Component {
  val io = new Bundle {
    val addr = in UInt(32 bits)
    val op = in(Uops())
    val raw_data = in UInt(32 bits)
    val strobe = out UInt(4 bits)
    val size = out UInt(3 bits)
    val isSigned = out Bool()
    val data_o = out UInt(32 bits) 
  }
  val addr10 = io.addr(1 downto 0)
  io.strobe := U"0000" // load
  io.size := MSIZE4
  io.data_o := U(0).resized
  io.isSigned := !(io.op === uOpLbu || io.op === uOpLhu) // unsigned => 0
  switch (io.op) {
    is(uOpSb) {
      io.size := MSIZE1
      switch(addr10) {
        is(1) {
          io.strobe := U"0010"
          io.data_o(15 downto 8) := io.raw_data(7 downto 0)
        }
        is(2) {
          io.strobe := U"0100"
          io.data_o(23 downto 16) := io.raw_data(7 downto 0)
        }
        is(3) {
          io.strobe := U"1000"
          io.data_o(31 downto 24) := io.raw_data(7 downto 0)
        }
        default { // 0
          io.strobe := U"0001"
          io.data_o(7 downto 0) := io.raw_data(7 downto 0)
        }
      }
    } // SB
    is(uOpSh) {
      io.size := MSIZE2
      switch (addr10) {
        is(2) {
          io.strobe := U"1100"
          io.data_o(31 downto 16) := io.raw_data(15 downto 0)
        }
        default { // 0
          io.strobe := U"0011"
          io.data_o(15 downto 0) := io.raw_data(15 downto 0)
        }
      }
    } // SH
    is(uOpSw) {
      io.size := MSIZE4
      io.strobe := U"1111"
      io.data_o := io.raw_data
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