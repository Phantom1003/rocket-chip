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
  override lazy val module = new PointerEncryptionSingleCycleImp(this)
}

class PointerEncryptionSingleCycleImp(outer: PointerEncryption)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
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

  val pec_engine = Module(new QarmaSingleCysle(max_round = 7))

  val keySelect = Cat(io.cmd.bits.inst.xd, io.cmd.bits.inst.xs1, io.cmd.bits.inst.xs2)

  pec_engine.input.bits.text  := io.cmd.bits.rs1
  pec_engine.input.bits.tweak := io.cmd.bits.rs2
  pec_engine.input.bits.actual_round := 7.U(3.W)
  pec_engine.input.bits.encrypt := ~io.cmd.bits.inst.funct(0)
  pec_engine.input.valid := io.cmd.fire()
  pec_engine.output.ready := true.B
  io.resp.bits.rd := io.cmd.bits.inst.rd
  io.resp.bits.data := pec_engine.output.bits.result

  pec_engine.input.bits.keyh := MuxLookup(keySelect, csr_scrtkeyh, Seq(
    "b000".U -> csr_scrtkeyh,
    "b001".U -> csr_mcrmkeyh,
    "b010".U -> csr_scrakeyh,
    "b011".U -> csr_scrbkeyh
  ))
  pec_engine.input.bits.keyl := MuxLookup(keySelect, csr_scrtkeyl, Seq(
    "b000".U -> csr_scrtkeyl,
    "b001".U -> csr_mcrmkeyl,
    "b010".U -> csr_scrakeyl,
    "b011".U -> csr_scrbkeyl
  ))

  io.cmd.ready := io.resp.ready
  io.busy := io.cmd.valid
  io.resp.valid := io.cmd.valid

  // Disable unused interfaces
  io.interrupt := false.B
  io.mem.req.valid := false.B
}

class PointerEncryptionMultiCycleImp(outer: PointerEncryption)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

//   val s_idle :: s_forward :: s_reflect :: s_backward :: s_resp :: Nil = Enum(4)
//   val state = RegInit(s_idle)

  // Disable unused interfaces
  io.interrupt := false.B
  io.mem.req.valid := false.B
}