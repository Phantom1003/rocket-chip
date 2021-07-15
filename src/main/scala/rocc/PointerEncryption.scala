package freechips.rocketchip.rocc.pec

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocc.qarma._

object PECInst {
  val opcode = BitPat("b?????????????????????????1101011")
}

class PointerEncryption(opcodes: OpcodeSet)(implicit p: Parameters) 
    extends LazyRoCC(opcodes) 
    with HasCoreParameters { 
  override lazy val module = new PointerEncryptionMultiCycleImp(this)
}

class PointerEncryptionMultiCycleImp(outer: PointerEncryption)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import chisel3.util.experimental.BoringUtils
  val csr_mcrmkeyl = WireInit(0.U(xLen.W))
  val csr_mcrmkeyh = WireInit(0.U(xLen.W))
  val csr_scrtkeyl = WireInit(0.U(xLen.W))
  val csr_scrtkeyh = WireInit(0.U(xLen.W))
  val csr_scrakeyl = WireInit(0.U(xLen.W))
  val csr_scrakeyh = WireInit(0.U(xLen.W))
  val csr_scrbkeyl = WireInit(0.U(xLen.W))
  val csr_scrbkeyh = WireInit(0.U(xLen.W))
  val flcsr = WireInit(false.B)
  val flSelect = WireInit(0.U(3.W))

  BoringUtils.addSink(csr_mcrmkeyl, "csr_mcrmkeyl")
  BoringUtils.addSink(csr_mcrmkeyh, "csr_mcrmkeyh")
  BoringUtils.addSink(csr_scrtkeyl, "csr_scrtkeyl")
  BoringUtils.addSink(csr_scrtkeyh, "csr_scrtkeyh")
  BoringUtils.addSink(csr_scrakeyl, "csr_scrakeyl")
  BoringUtils.addSink(csr_scrakeyh, "csr_scrakeyh")
  BoringUtils.addSink(csr_scrbkeyl, "csr_scrbkeyl")
  BoringUtils.addSink(csr_scrbkeyh, "csr_scrbkeyh")

  BoringUtils.addSink(flcsr, "csr_flcsr")
  BoringUtils.addSink(flSelect, "csr_flselect")

  val pec_engine = Module(new QarmaMultiCycle(7))
  val cache = Module(new QarmaCache(16, "Stack"))

  val rd = RegInit(0.U(5.W))
  val busy = RegInit(false.B)
  val resp = RegInit(false.B)
  val result = RegInit(0.U(xLen.W))
  val text  = RegInit(0.U(xLen.W))
  val tweak = RegInit(0.U(xLen.W))
  val ksel  = RegInit(0.U(3.W))
  val encrypt = RegInit(false.B)
  val keySelect = Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)

  // TODO begin
  cache.io.ren    := io.cmd.fire()
  cache.io.sel    := Mux(flcsr, flSelect, Mux(io.cmd.fire(), keySelect, ksel))
  cache.io.flush  := flcsr
  cache.io.update := pec_engine.output.valid
  cache.io.cipher := Mux(encrypt, pec_engine.output.bits.result, text)
  cache.io.plain  := Mux(encrypt, text, pec_engine.output.bits.result)
  cache.io.tweak  := Mux(pec_engine.output.valid, tweak, io.cmd.bits.rs2)
  cache.io.text   := io.cmd.bits.rs1
  cache.io.encrypt := ~io.cmd.bits.inst.funct(0)
  // TODO end
  pec_engine.input.bits.actual_round  := 7.U(3.W)
  pec_engine.input.bits.keyh  := MuxLookup(keySelect, csr_scrtkeyh, Seq(
      "b000".U -> csr_scrtkeyh,
      "b001".U -> csr_mcrmkeyh,
      "b010".U -> csr_scrakeyh,
      "b011".U -> csr_scrbkeyh
    ))
  pec_engine.input.bits.keyl  := MuxLookup(keySelect, csr_scrtkeyl, Seq(
      "b000".U -> csr_scrtkeyl,
      "b001".U -> csr_mcrmkeyl,
      "b010".U -> csr_scrakeyl,
      "b011".U -> csr_scrbkeyl
    ))
  pec_engine.input.bits.text  := io.cmd.bits.rs1
  pec_engine.input.bits.tweak := io.cmd.bits.rs2
  pec_engine.input.bits.encrypt := ~io.cmd.bits.inst.funct(0)
  pec_engine.input.valid   := io.cmd.fire() && !cache.io.hit

  pec_engine.output.ready  := pec_engine.output.valid

  when(io.cmd.fire()){
    busy := Mux(cache.io.hit, false.B, true.B)
    rd := io.cmd.bits.inst.rd
    text  := io.cmd.bits.rs1
    tweak := io.cmd.bits.rs2
    encrypt := ~io.cmd.bits.inst.funct(0)
    ksel := keySelect

    printf("[cmd fire] text %x tweak %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  }

  when (io.cmd.fire() && cache.io.hit) {
    result := cache.io.result
    resp := true.B
  }.elsewhen (pec_engine.output.valid) {
    result := pec_engine.output.bits.result
    resp := true.B

    printf("[pec valid] res %x\n", pec_engine.output.bits.result)
  }

  when(io.resp.fire()){
    resp := false.B
    busy := false.B
    result := 0.U
    
    printf("[resp fire]\n")
  } 

  io.resp.bits.rd   := Mux(io.cmd.fire() && cache.io.hit, io.cmd.bits.inst.rd, rd)
  io.resp.bits.data := Mux(io.cmd.fire() && cache.io.hit, cache.io.result, Mux(pec_engine.output.valid
, pec_engine.output.bits.result, result))

  io.cmd.ready  := !busy
  io.busy       := busy && !pec_engine.output.valid
  io.resp.valid := resp || (io.cmd.fire() && cache.io.hit) || pec_engine.output.valid

  // Disable unused interfaces
  io.interrupt      := false.B
  io.mem.req.valid  := false.B
}