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

  // cache
  dcache.io.creqs  <> arbiter51.io.dcreqs
  dcache.io.cresps <> arbiter51.io.dcresps
  icache.io.cresp  <> arbiter51.io.icresp
  icache.io.creq   <> arbiter51.io.icreq
  io.oresp <> arbiter51.io.cresp
  io.oreq  <> arbiter51.io.creq 

  icache.io.stall_all := controlFlow.io.outputs.icacheStall

  // control flow
  controlFlow.io.inputs.fetchBufferFull   := fetchBuffer.io.full
  controlFlow.io.inputs.singleIssue       := issue.io.prevStall
  controlFlow.io.inputs.branchPredictMiss := execute.io.redirectEn
  controlFlow.io.inputs.dcacheMiss        := mem.io.dcacheMiss
  controlFlow.io.inputs.memSingleIssue    := mem.io.singleIssueStall
  controlFlow.io.inputs.loadRawStall      := readRegfiles.io.loadRawStall
  controlFlow.io.inputs.cp0Redirect       := cp0Reg.io.redirectEn

  // fetch
  fetch.io.stall       := controlFlow.io.outputs.fetchStall
  fetch.io.icache <> icache.io.fetch_if
  fetch.io.exRedirectEn := execute.io.redirectEn
  fetch.io.exRedirectPc := execute.io.redirectPc
  fetch.io.cp0RedirectEn := cp0Reg.io.redirectEn
  fetch.io.cp0RedirectPc := cp0Reg.io.redirectPc

  // fetchBuffer
  fetchBuffer.io.pushInsts := fetch.io.insts
  fetchBuffer.io.flush     := controlFlow.io.outputs.fetchFlush
  fetchBuffer.io.stall     := controlFlow.io.outputs.fetchStall
  fetchBuffer.io.popStall  := controlFlow.io.outputs.fetchBufferPopStall

  // decode
  decoders(0).io.inst := RegNextWhen(fetchBuffer.io.popInsts(0), !controlFlow.io.outputs.decodeStall) init(0)
  decoders(1).io.inst := RegNextWhen(fetchBuffer.io.popInsts(1), !controlFlow.io.outputs.decodeStall) init(0)
  decoders(0).io.pc   := RegNextWhen(fetchBuffer.io.popPc(0)   , !controlFlow.io.outputs.decodeStall) init(0)
  decoders(1).io.pc   := RegNextWhen(fetchBuffer.io.popPc(1)   , !controlFlow.io.outputs.decodeStall) init(0)
  decoders.foreach(_.io.flush := execute.io.redirectEn)

  // issue & readRegfile
  issue.io.stall       := controlFlow.io.outputs.readrfStall
  issue.io.decodeInst0 := RegNextWhen(decoders(0).io.signals, !controlFlow.io.outputs.decodeStall) init(idu.DecodedSignals().nopDecodedSignals)
  issue.io.decodeInst1 := RegNextWhen(decoders(1).io.signals, !controlFlow.io.outputs.decodeStall) init(idu.DecodedSignals().nopDecodedSignals)
  readRegfiles.io.flush := execute.io.redirectEn || cp0Reg.io.redirectEn
  readRegfiles.io.decodedSignals(0) := issue.io.issueInst0
  readRegfiles.io.decodedSignals(1) := issue.io.issueInst1
  (readRegfiles.io.generalRegfile zip generalRegfile.io.read).foreach { case (read, readReg) => read <> readReg }
  (readRegfiles.io.exBypass zip execute.io.exBypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem1Bypass zip mem.io.mem1Bypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem2Bypass zip mem.io.mem2Bypass).foreach { case (a, b) => a := b }
  (readRegfiles.io.mem3Bypass zip mem.io.mem3Bypass).foreach { case (a, b) => a := b }

  // execute
  // execute.io.readrfSignals(0) := RegNext(readRegfiles(0).io.readrfSignals)
  // execute.io.readrfSignals(1) := RegNext(readRegfiles(1).io.readrfSignals)
  execute.io.readrfSignals  := RegNextWhen(readRegfiles.io.readrfSignals, !controlFlow.io.outputs.executeStall)
  execute.io.readrfSignals.foreach(_.init(idu.ReadRfSignals().nopReadRfSignals))
  execute.io.readrfPc       := issue.io.issueInst0.pc
  execute.io.writeHilo      <> hiloRegfile.io.write
  execute.io.hiloData       := hiloRegfile.io.hiloData

  // mem
  mem.io.executedSignals := RegNextWhen(execute.io.executedSignals, !controlFlow.io.outputs.executeStall)
  mem.io.executedSignals.foreach(_.init(exu.ExecutedSignals().nopExecutedSignals))
  when (cp0Reg.io.redirectEn) {
    mem.io.executedSignals.foreach(_ := exu.ExecutedSignals().nopExecutedSignals)
  }
  generalRegfile.io.write(0) <> mem.io.wrRegPorts(0)
  generalRegfile.io.write(1) <> mem.io.wrRegPorts(1)
  (mem.io.dcache zip cacheAccess.io.mem).foreach { case (a, b) => a <> b }
  
  // cacheAccess
  for (i <- 0 until 2) {
    dcache.io.dreqs(i)            := cacheAccess.io.dcache(i).req
    arbiter51.io.uncache_reqs(i)  := cacheAccess.io.uncache(i).req
    cacheAccess.io.dcache(i).rsp  := dcache.io.dresps(i)
    cacheAccess.io.uncache(i).rsp := arbiter51.io.uncache_resps(i)
  }
  cacheAccess.io.stall := controlFlow.io.outputs.memStall

  // Cp0
  cp0Reg.io.read    <> mem.io.rdCp0Port
  cp0Reg.io.write   <> mem.io.wrCp0Port
  cp0Reg.io.exptReq <> mem.io.exptReq

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