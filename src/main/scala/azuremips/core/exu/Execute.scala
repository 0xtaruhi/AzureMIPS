package azuremips.core.exu

import spinal.core._
import spinal.lib._

import azuremips.core._
import azuremips.core.Uops._
import azuremips.core.idu.ReadRfSignals
import azuremips.core.ExceptionCode._
import azuremips.core.cp0.ExptInfo
import azuremips.core.cache.CReq._


class ExecutedSignals extends Bundle {
  val wrRegEn   = Bool()
  val wrRegAddr = UInt(5 bits)
  val isLoad    = Bool()
  val wrMemEn   = Bool()
  val rdMemEn   = Bool()
  val signExt   = Bool()
  val memSize   = UInt(3 bits)
  val wrMemMask = UInt(4 bits)
  val wrHi      = Bool()
  val wrLo      = Bool()
  val memVAddr  = UInt(32 bits)
  val wrData    = UInt(32 bits)
  // val exptEn    = Bool()
  // val exptCode  = UInt(exptCodeWidth bits)
}

class Execute extends Component {
  val io = new Bundle {
    val readrfSignals   = in Vec(new ReadRfSignals, 2)
    val executedSignals = out Vec(new ExecutedSignals, 2)
    val except          = master(new ExptInfo)
    val readrfPc        = in UInt(32 bits)
    val redirectEn      = out Bool()
    val redirectPc      = out UInt(32 bits)
  }

  (io.readrfSignals zip io.executedSignals).foreach {
    case (readrf, execute) => {

      // Basic Arithmetic Instructions Result
      switch (readrf.uop) {
        is (uOpAdd, uOpAddu) {
          execute.wrData := readrf.op1Data + readrf.op2Data
        }
        is (uOpSub, uOpSubu) {
          execute.wrData := readrf.op1Data - readrf.op2Data
        }
        is (uOpSlt) {
          execute.wrData := Mux(S(readrf.op1Data) < S(readrf.op2Data), U(1), U(0)).resized
        }
        is (uOpSltu) {
          execute.wrData := Mux(readrf.op1Data < readrf.op2Data, U(1), U(0)).resized
        }
        is (uOpAnd) {
          execute.wrData := readrf.op1Data & readrf.op2Data
        }
        is (uOpOr) {
          execute.wrData := readrf.op1Data | readrf.op2Data
        }
        is (uOpXor) {
          execute.wrData := readrf.op1Data ^ readrf.op2Data
        }
        is (uOpLui) {
          execute.wrData := readrf.imm |<< 16
        }
        is (uOpNor) {
          execute.wrData := ~(readrf.op1Data | readrf.op2Data)
        }
        is (uOpSll) {
          execute.wrData := readrf.op2Data |<< readrf.imm(4 downto 0)
        }
        is (uOpSllv) {
          execute.wrData := readrf.op2Data |<< readrf.op1Data(4 downto 0)
        }
        is (uOpSrav) {
          execute.wrData := U(S(readrf.op2Data) |>> readrf.op1Data(4 downto 0))
        }
        is (uOpSra) {
          execute.wrData := U(S(readrf.op2Data) |>> readrf.imm(4 downto 0))
        }
        is (uOpSrl) {
          execute.wrData := readrf.op2Data |>> readrf.imm(4 downto 0)
        }
        is (uOpSrlv) {
          execute.wrData := readrf.op2Data |>> readrf.op1Data(4 downto 0)
        }
        is (uOpJal, uOpJalr, uOpBgezal, uOpBltzal) {
          execute.wrData := readrf.pc + 8
        }
        // form hi/lo or to hi/lo
        is (uOpMfhi, uOpMflo, uOpMthi, uOpMtlo) {
          execute.wrData := readrf.op1Data
        }
        // to memory
        // is (uOpSb) {
        //   execute.wrData := readrf.op2Data & U(0x07)
        // }
        // is (uOpSh) {
        //   execute.wrData := readrf.op2Data & U(0x0F)
        // }
        // is (uOpSw) {
        //   execute.wrData := readrf.op2Data
        // }
        default {
          execute.wrData := U(0)
        }
      }
      execute.wrRegAddr := readrf.wrRegAddr
      execute.memVAddr := readrf.imm + readrf.op1Data
      execute.wrRegEn := readrf.wrRegEn

      switch (readrf.uop) {
        is (uOpLb, uOpLbu, uOpLh, uOpLhu, uOpLw) {
          execute.wrMemEn := False
          execute.rdMemEn := True
        }
        is (uOpSb, uOpSh, uOpSw) {
          execute.wrMemEn := True
          execute.rdMemEn := False
        }
        default {
          execute.wrMemEn := False
          execute.rdMemEn := False
        }
      } // rd/wr MemEn

      execute.wrHi := False
      execute.wrLo := False
      when (readrf.uop === uOpMthi) {
        execute.wrHi := True
      }
      when (readrf.uop === uOpMtlo) {
        execute.wrLo := True
      }
      when (readrf.uop === uOpMult || readrf.uop === uOpMultu ||
            readrf.uop === uOpDiv  || readrf.uop === uOpDivu) {
        execute.wrHi := True
        execute.wrLo := True
      }

      // branch
      val shouldJmp = False
      val jmpDestPc = U(0, 32 bits)
      switch (readrf.uop) {
        is (uOpBeq) {
          shouldJmp := readrf.op1Data === readrf.op2Data
        }
        is (uOpBne) {
          shouldJmp := readrf.op1Data =/= readrf.op2Data
        }
        is (uOpBgez, uOpBgezal) {
          shouldJmp := readrf.op1Data.msb === False
        }
        is (uOpBgtz) {
          shouldJmp := S(readrf.op1Data) > S(0)
        }
        is (uOpBlez) {
          shouldJmp := S(readrf.op1Data) <= S(0)
        }
        is (uOpBltz, uOpBltzal) {
          shouldJmp := S(readrf.op1Data) < S(0)
        }
        is (uOpJ, uOpJal, uOpJalr, uOpJr) {
          shouldJmp := True
        }
      }
      switch (readrf.uop) {
        is (uOpBeq, uOpBne, uOpBgez, uOpBgezal, uOpBgtz, uOpBlez, uOpBltz, uOpBltzal) {
          jmpDestPc := readrf.pc + 4 + readrf.imm
        }
        is (uOpJ, uOpJal) {
          jmpDestPc := readrf.imm
        }
        is (uOpJr, uOpJalr) {
          jmpDestPc := readrf.op1Data
        }
      }

      switch (readrf.uop) {
        is (uOpBgezal, uOpBltzal, uOpJal, uOpJalr) {
          execute.wrData := readrf.pc + 8
        }
      }

      when (shouldJmp && jmpDestPc =/= io.readrfPc) {
        io.redirectEn := True
        io.redirectPc := jmpDestPc
      } otherwise {
        io.redirectEn := False
        io.redirectPc := 0
      }

      switch (readrf.uop) {
        is (uOpLw, uOpLh, uOpLhu, uOpLb, uOpLbu) {
          execute.isLoad := True
        }
        default {
          execute.isLoad := False
        }
      }

    }
    
    val genStrobeInst = new GenStrobe()
    execute.wrMemMask := genStrobeInst.io.strobe
    execute.memSize := genStrobeInst.io.size
    execute.signExt := genStrobeInst.io.isSigned
    genStrobeInst.io.addr := execute.memVAddr
    genStrobeInst.io.op := readrf.uop
  }

  

  // exception 
  val instsExptValid = Vec(Bool(), 2)
  val instsExptCode  = Vec(UInt(exptCodeWidth bits), 2)
  for (i <- 0 until 2) {
    instsExptValid(i) := False
    instsExptCode(i) := 0

    switch (io.readrfSignals(i).uop) {
      is (uOpAdd) {
        when (io.readrfSignals(i).op1Data(31) === io.readrfSignals(i).op2Data(31)) {
          when (io.readrfSignals(i).op1Data(31) =/= io.executedSignals(i).wrData(31)) {
            instsExptValid(i) := True
            instsExptCode(i) := EXC_OVF
          }
        }
      }
      is (uOpSub) {
        when (io.readrfSignals(i).op1Data(31) =/= io.readrfSignals(i).op2Data(31)) {
          when (io.readrfSignals(i).op1Data(31) =/= io.executedSignals(i).wrData(31)) {
            instsExptValid(i) := True
            instsExptCode(i) := EXC_OVF
          }
        }
      }
      is (uOpSh) {
        when (io.executedSignals(i).memVAddr(0) =/= False) {
          instsExptValid(i) := True
          instsExptCode(i) := EXC_ADES
        }
      }
      is (uOpSw) {
        when (io.executedSignals(i).memVAddr(1 downto 0) =/= U"00") {
          instsExptValid(i) := True
          instsExptCode(i) := EXC_ADES
        }
      }
      is (uOpLh, uOpLhu) {
        when (io.executedSignals(i).memVAddr(0) =/= False) {
          instsExptValid(i) := True
          instsExptCode(i) := EXC_ADEL
        }
      }
      is (uOpLw) {
        when (io.executedSignals(i).memVAddr(1 downto 0) =/= U"00") {
          instsExptValid(i) := True
          instsExptCode(i) := EXC_ADEL
        }
      }
      is (uOpSyscall) {
        instsExptValid(i) := True
        instsExptCode(i) := EXC_SYSCALL
      }
      is (uOpBreak) {
        instsExptValid(i) := True
        instsExptCode(i) := EXC_BREAK
      }

      default {
        instsExptValid(i) := False
        instsExptCode(i) := 0
      }
    }

    // when (io.readrfSignals(i).validInst === False) {
    //   instsExptValid(i) := True
    //   instsExptCode(i) := EXC_RESERVED
    // }
  }

  val inst0isBr = {
    val inst0 = io.readrfSignals(0) 
    inst0.uop === uOpBeq || inst0.uop === uOpBne ||
    inst0.uop === uOpBgez || inst0.uop === uOpBgezal ||
    inst0.uop === uOpBgtz || inst0.uop === uOpBlez ||
    inst0.uop === uOpBltz || inst0.uop === uOpBltzal ||
    inst0.uop === uOpJ || inst0.uop === uOpJal ||
    inst0.uop === uOpJr || inst0.uop === uOpJalr
  }

  when (instsExptValid(0)) {
    io.except.exptValid := True
    io.except.exptCode  := instsExptCode(0)
    io.except.pc        := io.readrfSignals(0).pc
    io.except.inBD      := False
  } elsewhen (instsExptValid(1)) {
    io.except.exptValid := True
    io.except.exptCode := instsExptCode(1)
    when (inst0isBr) {
      io.except.pc := io.readrfSignals(0).pc
      io.except.inBD := True
    } otherwise {
      io.except.pc := io.readrfSignals(1).pc
      io.except.inBD := False
    }
  } otherwise {
    io.except.pc := 0
    io.except.exptCode := 0
    io.except.exptValid := False
    io.except.inBD := False
  }

  when (io.except.exptValid) {
    io.redirectEn := True
    io.redirectPc := U"32'hbfc00380"
  }
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