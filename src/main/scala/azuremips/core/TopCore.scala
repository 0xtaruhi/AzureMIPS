package azuremips.core

import spinal.core._
import spinal.lib._

import azuremips.core.cache._

case class TopCore(config: CoreConfig = CoreConfig()) extends Component {
  val io  = new Bundle {
    val oreq = out(new CReq())
    val oresp = in(new CResp())
    val ext_int = in(UInt(6 bits)) // exception signal
  }

  noIoPrefix() // can connect VTop.sv with (.*)

  val dcache = new DCache()
  val icache = new ICache()
  val arbiter51 = new CBusArbiter51()
  val fetch          = new ifu.Fetch
  val fetchBuffer    = new ifu.FetchBuffer(16)
  val decoders       = for (i <- 0 until 2) yield { new idu.Decoder }
  // val readRegfiles   = for (i <- 0 until 2) yield { new idu.ReadRegfile }
  val readRegfiles   = new idu.ReadRegfiles
  val execute        = new exu.Execute
  val issue          = new idu.Issue
  val generalRegfile = new reg.GeneralRegfile
  val hiloRegfile    = new reg.HiloRegfile
  val mem            = new lsu.Mem
  val cacheAccess    = new lsu.CacheAccess
  val controlFlow    = new ControlFlow
  val cp0Reg         = new cp0.Cp0
  val tlb            = new mmu.Tlb

  val regRedirectEnExMem = RegNext(execute.io.redirectEn) init(False)
  val regRedirectPcExMem = RegNext(execute.io.redirectPc) init(0) // actual target
  val regUpdateEnExMem   = RegNext(execute.io.executedSignals(0).isBr) init(False)
  val regUpdateTakenExMem = RegNext(execute.io.updateTaken) init(False)
  val regUpdatePcExMem   = RegNext(execute.io.executedSignals(0).pc) init(0)
  val regICacheInstInfo  = RegNext(execute.io.icacheInstInfo) init(CacheInstInfo.emptyCacheInstInfo)
  val regICacheInstVAddr = RegNext(execute.io.icacheInstVAddr) init (0)

  // cache
  dcache.io.creqs  <> arbiter51.io.dcreqs
  dcache.io.cresps <> arbiter51.io.dcresps
  icache.io.cresp  <> arbiter51.io.icresp
  icache.io.creq   <> arbiter51.io.icreq
  dcache.io.cache_inst_info     := cacheAccess.io.dcacheInst
  dcache.io.tag_for_index_store := cacheAccess.io.dcacheIndexStoreTag
  icache.io.cache_inst_info     := regICacheInstInfo
  icache.io.tag_for_index_store := 0
  io.oresp <> arbiter51.io.cresp
  io.oreq  <> arbiter51.io.creq 

  // control flow
  controlFlow.io.inputs.fetchBufferFull   := fetchBuffer.io.full
  controlFlow.io.inputs.singleIssue       := issue.io.prevStall
  controlFlow.io.inputs.branchPredictMiss := regRedirectEnExMem
  controlFlow.io.inputs.dcacheMiss        := mem.io.dcacheMiss
  controlFlow.io.inputs.memSingleIssue    := mem.io.singleIssueStall
  controlFlow.io.inputs.loadRawStall      := readRegfiles.io.loadRawStall
  controlFlow.io.inputs.multiCycleStall   := execute.io.multiCycleStall
  controlFlow.io.inputs.cp0Redirect       := cp0Reg.io.redirectEn

  // fetch
  fetch.io.stall       := controlFlow.io.outputs.fetchStall
  fetch.io.icache <> icache.io.fetch_if
  fetch.io.exRedirectEn := regRedirectEnExMem
  fetch.io.exRedirectPc := regRedirectPcExMem
  fetch.io.cp0RedirectEn := cp0Reg.io.redirectEn
  fetch.io.cp0RedirectPc := cp0Reg.io.redirectPc
  fetch.io.updateEn      := regUpdateEnExMem
  fetch.io.updatePc      := regUpdatePcExMem
  fetch.io.updateTaken   := regUpdateTakenExMem
  fetch.io.tlbPort       <> tlb.io.trans(2)
  fetch.io.icacheInstValid := regICacheInstInfo.isCacheInst
  fetch.io.icacheInstVAddr := regICacheInstVAddr

  // fetchBuffer
  fetchBuffer.io.pushInsts := fetch.io.insts
  fetchBuffer.io.flush     := controlFlow.io.outputs.fetchFlush
  fetchBuffer.io.stall     := controlFlow.io.outputs.fetchStall
  fetchBuffer.io.popStall  := controlFlow.io.outputs.fetchBufferPopStall
  fetchBuffer.io.multiCycleStall := execute.io.multiCycleStall

  // decode
  val regPredictTargetFbDe = RegNextWhen(fetchBuffer.io.predictTarget, !controlFlow.io.outputs.decodeStall) init(0)
  decoders(0).io.inst := RegNextWhen(fetchBuffer.io.popInsts(0), !controlFlow.io.outputs.decodeStall) init(0)
  decoders(1).io.inst := RegNextWhen(fetchBuffer.io.popInsts(1), !controlFlow.io.outputs.decodeStall) init(0)
  decoders(0).io.pc   := RegNextWhen(fetchBuffer.io.popPc(0)   , !controlFlow.io.outputs.decodeStall) init(0)
  decoders(1).io.pc   := RegNextWhen(fetchBuffer.io.popPc(1)   , !controlFlow.io.outputs.decodeStall) init(0)
  decoders(0).io.tlbInfo := RegNextWhen(fetchBuffer.io.popTlbInfo(0), !controlFlow.io.outputs.decodeStall) init(0)
  decoders(1).io.tlbInfo := RegNextWhen(fetchBuffer.io.popTlbInfo(1), !controlFlow.io.outputs.decodeStall) init(0)
  decoders.foreach(_.io.flush := regRedirectEnExMem)

  // issue & readRegfile
  issue.io.stall       := controlFlow.io.outputs.readrfStall
  val regPredictTargetDeIss = RegNextWhen(regPredictTargetFbDe, !controlFlow.io.outputs.decodeStall) init(0)
  issue.io.decodeInst0 := RegNextWhen(decoders(0).io.signals, !controlFlow.io.outputs.decodeStall) init(idu.DecodedSignals().nopDecodedSignals)
  issue.io.decodeInst1 := RegNextWhen(decoders(1).io.signals, !controlFlow.io.outputs.decodeStall) init(idu.DecodedSignals().nopDecodedSignals)
  readRegfiles.io.flush := regRedirectEnExMem || cp0Reg.io.redirectEn || execute.io.redirectEn
  readRegfiles.io.decodedSignals(0) := issue.io.issueInst0
  readRegfiles.io.decodedSignals(1) := issue.io.issueInst1
  (readRegfiles.io.generalRegfile zip generalRegfile.io.read).foreach { case (read, readReg) => read <> readReg }
  (readRegfiles.io.exBypass zip execute.io.exBypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem1Bypass zip mem.io.mem1Bypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem2Bypass zip mem.io.mem2Bypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem3Bypass zip mem.io.mem3Bypass).foreach { case (a, b) => a := b }

  // execute
  val regPredictTargetRfEx    = RegNextWhen(regPredictTargetDeIss, !controlFlow.io.outputs.executeStall && !execute.io.multiCycleStall) init(0)
  execute.io.readrfSignals   := RegNextWhen(readRegfiles.io.readrfSignals, !controlFlow.io.outputs.executeStall && !execute.io.multiCycleStall)
  execute.io.jmpDestPc       := RegNextWhen(readRegfiles.io.jmpDestPc, !controlFlow.io.outputs.executeStall && !execute.io.multiCycleStall) init(0)
  execute.io.readrfSignals.foreach(_.init(idu.ReadRfSignals().nopReadRfSignals))
  execute.io.predictTarget   := regPredictTargetRfEx
  execute.io.writeHilo       <> hiloRegfile.io.write
  execute.io.hiloData        := hiloRegfile.io.hiloData
  execute.io.multiCycleFlush := controlFlow.io.outputs.multiCycleFlush

  // mem
  val regExMem = RegNext(execute.io.executedSignals)
  regExMem.foreach(_.init(exu.ExecutedSignals().nopExecutedSignals))
  when (cp0Reg.io.redirectEn || cp0Reg.io.hwIntTrig) {
    regExMem.map(x => x := exu.ExecutedSignals().nopExecutedSignals)
  }.elsewhen(controlFlow.io.outputs.executeStall) {
    regExMem := regExMem
  }.elsewhen(execute.io.multiCycleStall || regRedirectEnExMem) {
    regExMem.map(x => x := exu.ExecutedSignals().nopExecutedSignals)
  }
  // val regRedirectEnExMem = RegNext(execute.io.redirectEn) init(False)
  // val regRedirectPcExMem = RegNext(execute.io.redirectPc) init(0)
  val regAddrConflictExMem = RegNext(execute.io.addrConflict) init(False)
  when (cp0Reg.io.redirectEn) {
    regRedirectEnExMem := False
    regRedirectPcExMem := 0
    regUpdateEnExMem   := False
    regUpdateTakenExMem := False
    regAddrConflictExMem := False
    regUpdatePcExMem   := 0
    regICacheInstInfo  := CacheInstInfo.emptyCacheInstInfo
    regICacheInstVAddr := 0 // todo: should be optimized
  }.elsewhen(controlFlow.io.outputs.executeStall) {
    regRedirectEnExMem := regRedirectEnExMem
    regRedirectPcExMem := regRedirectPcExMem
    regUpdateEnExMem   := regUpdateEnExMem
    regUpdateTakenExMem := regUpdateTakenExMem
    regUpdatePcExMem   := regUpdatePcExMem
    regAddrConflictExMem := regAddrConflictExMem
    regICacheInstInfo  := regICacheInstInfo
    regICacheInstVAddr := regICacheInstVAddr
  }.elsewhen(execute.io.multiCycleStall || regRedirectEnExMem) {
    regRedirectEnExMem := False
    regRedirectPcExMem := 0
    regUpdateEnExMem   := False
    regUpdateTakenExMem := False
    regUpdatePcExMem   := 0
    regAddrConflictExMem := False
    regICacheInstInfo := CacheInstInfo.emptyCacheInstInfo
    regICacheInstVAddr := 0
  }

  mem.io.executedSignals := regExMem
  mem.io.addrConflict    := regAddrConflictExMem
  generalRegfile.io.write(0) <> mem.io.wrRegPorts(0)
  generalRegfile.io.write(1) <> mem.io.wrRegPorts(1)
  (mem.io.dcache zip cacheAccess.io.mem).foreach { case (a, b) => a <> b }
  mem.io.rdCp0Data := cp0Reg.io.read.data
  mem.io.hwIntTrig := cp0Reg.io.hwIntTrig
  
  // cacheAccess
  for (i <- 0 until 2) {
    dcache.io.dreqs(i)            := cacheAccess.io.dcache(i).req
    arbiter51.io.uncache_reqs(i)  := cacheAccess.io.uncache(i).req
    cacheAccess.io.dcache(i).rsp  := dcache.io.dresps(i)
    cacheAccess.io.uncache(i).rsp := arbiter51.io.uncache_resps(i)
  }
  cacheAccess.io.stall := controlFlow.io.outputs.memStall
  cacheAccess.io.tlbPort(0) <> tlb.io.trans(0)
  cacheAccess.io.tlbPort(1) <> tlb.io.trans(1)

  // Cp0
  // cp0Reg.io.read    <> mem.io.rdCp0Port
  cp0Reg.io.read.addr := mem.io.rdCp0Addr
  cp0Reg.io.read.sel  := mem.io.rdCp0Sel
  cp0Reg.io.write   <> mem.io.wrCp0Port
  cp0Reg.io.exptReq <> mem.io.exptReq
  cp0Reg.io.hwInterrupt := RegNext(io.ext_int) init(0)
  cp0Reg.io.hwIntMemAvail := mem.io.hwIntAvail
  cp0Reg.io.tlbWen    := execute.io.tlbWen
  cp0Reg.io.tlbRen    := execute.io.tlbRen
  cp0Reg.io.tlbProbeEn:= execute.io.tlbProbe

  // TLB
  tlb.io.asid  := cp0Reg.io.asid
  tlb.io.write <> cp0Reg.io.tlbWrite
  tlb.io.read  <> cp0Reg.io.tlbRead
  tlb.io.probe <> cp0Reg.io.tlbProbe
}

object TopCore {
  def main(args: Array[String]) {
    // SpinalVerilog(ICache(CoreConfig()))
    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC)
    ).addStandardMemBlackboxing(blackboxAll)
    .generateVerilog(new TopCore)
  }
}