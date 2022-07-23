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

  dcache.io.creqs  <> arbiter51.io.dcreqs
  dcache.io.cresps <> arbiter51.io.dcresps
  icache.io.cresp  <> arbiter51.io.icresp
  icache.io.creq   <> arbiter51.io.icreq
  io.oresp <> arbiter51.io.cresp
  io.oreq  <> arbiter51.io.creq

  val fetch          = new ifu.Fetch
  val fetchBuffer    = new ifu.FetchBuffer(16)
  val decoders       = for (i <- 0 until 2) yield { new idu.Decoder }
  val readRegfiles   = for (i <- 0 until 2) yield { new idu.ReadRegfile }
  val execute        = new exu.Execute
  val issue          = new idu.Issue
  val generalRegfile = new reg.GeneralRegfile
  val hiloRegfile    = new reg.HiloRegfile
  val mem            = new lsu.Mem
  val cacheAccess    = new lsu.CacheAccess
  val controlFlow    = new ControlFlow

  // control flow
  controlFlow.io.inputs.fetchBufferFull   := fetchBuffer.io.full
  controlFlow.io.inputs.singleIssue       := issue.io.prevStall
  controlFlow.io.inputs.branchPredictMiss := execute.io.redirectEn
  controlFlow.io.inputs.dcacheMiss        := mem.io.dcacheMiss
  controlFlow.io.inputs.loadRawStall      := readRegfiles.map(_.io.loadRawStall).reduce(_ || _)

  // fetch
  fetch.io.stall  := controlFlow.io.outputs.fetchStall
  fetch.io.icache <> icache.io.fetch_if
  fetch.io.exRedirectEn := execute.io.redirectEn
  fetch.io.exRedirectPc := execute.io.redirectPc

  // fetchBuffer
  fetchBuffer.io.pushInsts := fetch.io.insts
  fetchBuffer.io.flush     := controlFlow.io.outputs.fetchFlush
  fetchBuffer.io.stall     := controlFlow.io.outputs.fetchStall

  // decode
  decoders(0).io.inst := RegNext(fetchBuffer.io.popInsts(0))
  decoders(1).io.inst := RegNext(fetchBuffer.io.popInsts(1))
  decoders(0).io.pc   := RegNext(fetchBuffer.io.popPc(0))
  decoders(1).io.pc   := RegNext(fetchBuffer.io.popPc(1))

  // issue & readRegfile
  issue.io.decodeInst0 := RegNext(decoders(0).io.signals)
  issue.io.decodeInst1 := RegNext(decoders(1).io.signals)
  readRegfiles(0).io.decodedSignals := issue.io.issueInst0
  readRegfiles(1).io.decodedSignals := issue.io.issueInst1
  readRegfiles(0).io.generalRegfile(0) <> generalRegfile.io.read(0)
  readRegfiles(0).io.generalRegfile(1) <> generalRegfile.io.read(1)
  readRegfiles(1).io.generalRegfile(0) <> generalRegfile.io.read(2)
  readRegfiles(1).io.generalRegfile(1) <> generalRegfile.io.read(3)
  readRegfiles.foreach { rrf => {
    (rrf.io.exBypass zip execute.io.exBypass).foreach { case (a, b) => a := b }
    (rrf.io.mem1Bypass zip mem.io.mem1Bypass).foreach { case (a, b) => a := b }
    (rrf.io.mem2Bypass zip mem.io.mem2Bypass).foreach { case (a, b) => a := b }
    (rrf.io.mem3Bypass zip mem.io.mem3Bypass).foreach { case (a, b) => a := b }
  }}

  // execute
  execute.io.readrfSignals(0) := RegNext(readRegfiles(0).io.readrfSignals)
  execute.io.readrfSignals(1) := RegNext(readRegfiles(1).io.readrfSignals)
  execute.io.readrfPc         := readRegfiles(0).io.readrfSignals.pc
  execute.io.writeHilo        <> hiloRegfile.io.write
  execute.io.hiloData         := hiloRegfile.io.hiloData

  // mem
  mem.io.executedSignals(0) := RegNext(execute.io.executedSignals(0))
  mem.io.executedSignals(1) := RegNext(execute.io.executedSignals(1))
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