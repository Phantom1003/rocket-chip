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

  BoringUtils.addSink(csr_mcrmkeyl, "csr_mcrmkeyl")
  BoringUtils.addSink(csr_mcrmkeyh, "csr_mcrmkeyh")
  BoringUtils.addSink(csr_scrtkeyl, "csr_scrtkeyl")
  BoringUtils.addSink(csr_scrtkeyh, "csr_scrtkeyh")
  BoringUtils.addSink(csr_scrakeyl, "csr_scrakeyl")
  BoringUtils.addSink(csr_scrakeyh, "csr_scrakeyh")
  BoringUtils.addSink(csr_scrbkeyl, "csr_scrbkeyl")
  BoringUtils.addSink(csr_scrbkeyh, "csr_scrbkeyh")

  val pec_engine = Module(new QarmaMultiCycle(7))
  val cache = Module(new QarmaCache(8, "Stack"))

  val rd = RegInit(0.U(5.W))
  val busy = RegInit(false.B)
  val resp = RegInit(false.B)
  val result = RegInit(0.U(xLen.W))
  val text  = RegInit(0.U(xLen.W))
  val tweak = RegInit(0.U(xLen.W))
  val keyh = RegInit(0.U(xLen.W))
  val keyl = RegInit(0.U(xLen.W))
  val encrypt = RegInit(false.B)
  val valid = RegInit(false.B)

  cache.io.update := pec_engine.output.valid && valid
  cache.io.cipher := Mux(encrypt, result, text)
  cache.io.plain  := Mux(encrypt, text, result)
  cache.io.tweak  := tweak
  cache.io.keyh   := keyh
  cache.io.keyl   := keyl
  cache.io.text   := text
  cache.io.encrypt := encrypt
  pec_engine.input.bits.actual_round  := 7.U(3.W)
  pec_engine.input.bits.keyh  := keyh
  pec_engine.input.bits.keyl  := keyl
  pec_engine.input.bits.text  := text
  pec_engine.input.bits.tweak := tweak
  pec_engine.input.bits.encrypt := encrypt
  pec_engine.input.valid   := valid && !cache.io.hit

  pec_engine.output.ready  := pec_engine.output.valid

  when(io.cmd.fire()){
    valid := true.B
    busy := true.B
    rd := io.cmd.bits.inst.rd
    val keySelect = Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)
    keyh := MuxLookup(keySelect, csr_scrtkeyh, Seq(
      "b000".U -> csr_scrtkeyh,
      "b001".U -> csr_mcrmkeyh,
      "b010".U -> csr_scrakeyh,
      "b011".U -> csr_scrbkeyh
    ))
    keyl := MuxLookup(keySelect, csr_scrtkeyl, Seq(
      "b000".U -> csr_scrtkeyl,
      "b001".U -> csr_mcrmkeyl,
      "b010".U -> csr_scrakeyl,
      "b011".U -> csr_scrbkeyl
    ))
    text  := io.cmd.bits.rs1
    tweak := io.cmd.bits.rs2
    encrypt := ~io.cmd.bits.inst.funct(0)

    printf("[cmd fire] text %x tweak %x\n", io.cmd.bits.rs1, io.cmd.bits.rs2)
  }

  when (valid && cache.io.hit) {
    result := cache.io.result
    resp := true.B
  }.elsewhen (pec_engine.output.valid) {
    result := pec_engine.output.bits.result
    resp := true.B

    printf("[pec valid] res %x\n", pec_engine.output.bits.result)
  }

  when (valid) {
    valid := false.B    
  }

  when(io.resp.fire()){
    resp := false.B
    busy := false.B      
    valid := false.B
    result := 0.U
    
    printf("[resp fire]\n")
  } 

  io.resp.bits.rd   := rd
  io.resp.bits.data := result

  io.cmd.ready  := !busy
  io.busy       := busy
  io.resp.valid := resp

  // Disable unused interfaces
  io.interrupt      := false.B
  io.mem.req.valid  := false.B
}