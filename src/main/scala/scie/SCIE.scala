// See LICENSE.SiFive for license details.

package freechips.rocketchip.scie

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import freechips.rocketchip.rocket.CSRs._

/* Pointer Encryption Extension
 *          31    5 4  20  9   5 4 2 11  7 6     0
 *          |     | |   |  |   | | | |   | |     |
 * cretk    0000000 .....  ..... 000 ..... 1101011
 * crdtk    0000001 .....  ..... 000 ..... 1101011
 * cremk    0000000 .....  ..... 001 ..... 1101011
 * crdmk    0000001 .....  ..... 001 ..... 1101011
 * creak    0000000 .....  ..... 010 ..... 1101011
 * crdak    0000001 .....  ..... 010 ..... 1101011
 * crebk    0000000 .....  ..... 011 ..... 1101011
 * crdbk    0000001 .....  ..... 011 ..... 1101011
 */

object SCIE {
  val opcode = BitPat("b?????????????????????????1101011")
  val iLen = 32
}

class SCIEDecoderInterface extends Bundle {
  val insn = Input(UInt(SCIE.iLen.W))
  val unpipelined = Output(Bool())
  val pipelined = Output(Bool())
  val multicycle = Output(Bool())
}

class SCIEDecoder extends Module {
  val io = IO(new SCIEDecoderInterface)

  io.unpipelined  := true.B
  io.pipelined    := false.B
  io.multicycle   := false.B
}

class SCIEUnpipelinedInterface(xLen: Int) extends Bundle {
  val valid = Input(Bool())
  val kill = Input(Bool())
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
  val ready = Output(Bool())
  val expt = Output(Bool())
}

class SCIEUnpipelined(xLen: Int) extends Module {
  val io = IO(new SCIEUnpipelinedInterface(xLen))

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

  val pec_engine = Module(new Qarma.MultiCycle.QarmaEngine(max_round = 7))
  val cache = Module(new Qarma.MultiCycle.QarmaCache(16, "Stack"))

  val key_sel = io.insn(14, 12)
  
  def truncate_and_extend(): UInt = {
    val result = WireDefault(io.rs1)
    for(i <- 0 to 7) {
      for(j <- i to 7) {
        when(io.insn(28, 26) === i.U && io.insn(31, 29) === j.U) {
          if(i == 0 || j == 7) {
            if(i == 0 && j == 7) {
              result := io.rs1(8 * (j + 1) - 1, 8 * i)
            } else if (i == 0) {
              result := Cat(Fill(8 * (7 - j), 0.U), io.rs1(8 * (j + 1) - 1, 8 * i))
            } else {
              result := Cat(io.rs1(8 * (j + 1) - 1, 8 * i), Fill(8 * i, 0.U))
            }
          } else {
            result := Cat(Fill(8 * (7 - j), 0.U), io.rs1(8 * (j + 1) - 1, 8 * i), Fill(8 * i, 0.U))
          }
        }
      }
    }
    result
  }

  def check_integrity(): Bool = {
    val result = WireDefault(true.B)
    for (i <- 1 to 7) {
      when(io.insn(28, 26) === i.U) {
        val integrity = io.rd(8 * i - 1, 0).andR || !(io.rd(8 * i - 1, 0).orR)
        when (!integrity) {
          result := false.B
        }
      }
    }
    for (i <- 0 to 6) {
      when(io.insn(31, 29) === i.U) {
        val integrity = io.rd(63, 8 * (i + 1)).andR || !(io.rd(63, 8 * (i + 1)).orR)
        when(!integrity) {
          result := false.B
        }
      }
    }
    // for(i <- 0 to 7) {
    //   for(j <- i to 7) {
    //     when(io.insn(28, 26) === i.U && io.insn(31, 29) === j.U) {
    //       result := io.rd(8 * (j + 1) - 1, 8 * i) === 0.U || io.rd(8 * (j + 1) - 1, 8 * i).andR
    //     }
    //   }
    // }
    result
  }

  // Reset keys according to the select when CSRW a cryptographic key
  val rst_key = io.insn(6, 0) === "b1110011".U && io.insn(13, 12) =/= "b00".U
  val rst_sel = MuxLookup(io.insn(31, 20), "b111".U,
    Seq(
      mcrmkeyl.U -> "b001".U,
      mcrmkeyh.U -> "b001".U,
      scrtkeyl.U -> "b000".U,
      scrtkeyh.U -> "b000".U,
      scrakeyl.U -> "b010".U,
      scrakeyh.U -> "b010".U,
      scrbkeyl.U -> "b011".U,
      scrbkeyh.U -> "b011".U
    )
  )

  cache.io.flush  := rst_key
  cache.io.update := pec_engine.output.valid && io.valid
  cache.io.cipher := Mux(~io.insn(25), io.rd, io.rs1)
  cache.io.plain  := Mux(~io.insn(25), truncate_and_extend(), io.rd)
  cache.io.tweak  := io.rs2
  cache.io.sel    := Mux(rst_key, rst_sel, key_sel)
  cache.io.text   := Mux(~io.insn(25), truncate_and_extend(), io.rs1)
  cache.io.encrypt := ~io.insn(25)
  cache.io.ren    := io.valid && !pec_engine.output.valid

  pec_engine.kill.valid := false.B// RegNext(cache.io.hit) && RegNext(io.valid)
  pec_engine.input.bits.kill := io.kill
  pec_engine.input.bits.text := Mux(~io.insn(25), truncate_and_extend(), io.rs1)
  pec_engine.input.bits.tweak := io.rs2
  pec_engine.input.bits.actual_round := 7.U(3.W)
  pec_engine.input.bits.encrypt := ~io.insn(25)
  pec_engine.output.ready := true.B
  pec_engine.input.valid := io.valid && !cache.io.hit

  pec_engine.input.bits.keyh := MuxLookup(key_sel, csr_scrtkeyh, Seq(
    "b000".U -> csr_scrtkeyh,
    "b001".U -> csr_mcrmkeyh,
    "b010".U -> csr_scrakeyh,
    "b011".U -> csr_scrbkeyh
  ))
  pec_engine.input.bits.keyl := MuxLookup(key_sel, csr_scrtkeyl, Seq(
    "b000".U -> csr_scrtkeyl,
    "b001".U -> csr_mcrmkeyl,
    "b010".U -> csr_scrakeyl,
    "b011".U -> csr_scrbkeyl
  ))

  io.rd := Mux(cache.io.hit, cache.io.result, pec_engine.output.bits.result)
  io.ready := pec_engine.output.valid || cache.io.hit
  io.expt := io.valid && io.ready && io.insn(25) && (check_integrity() === false.B)
}

class SCIEPipelinedInterface(xLen: Int) extends Bundle {
  val clock = Input(Clock())
  val valid = Input(Bool())
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
  val done = Output(Bool())
}

class SCIEPipelined(xLen: Int) extends Module {
  val io = IO(new SCIEPipelinedInterface(xLen))

  io.rd := "hdeadbeef".U
  io.done := false.B
  // val csr_mcrmkeyl = WireInit(0.U(xLen.W))
  // val csr_mcrmkeyh = WireInit(0.U(xLen.W))
  // val csr_scrtkeyl = WireInit(0.U(xLen.W))
  // val csr_scrtkeyh = WireInit(0.U(xLen.W))
  // val csr_scrakeyl = WireInit(0.U(xLen.W))
  // val csr_scrakeyh = WireInit(0.U(xLen.W))
  // val csr_scrbkeyl = WireInit(0.U(xLen.W))
  // val csr_scrbkeyh = WireInit(0.U(xLen.W))
  // BoringUtils.addSink(csr_mcrmkeyl, "csr_mcrmkeyl")
  // BoringUtils.addSink(csr_mcrmkeyh, "csr_mcrmkeyh")
  // BoringUtils.addSink(csr_scrtkeyl, "csr_scrtkeyl")
  // BoringUtils.addSink(csr_scrtkeyh, "csr_scrtkeyh")
  // BoringUtils.addSink(csr_scrakeyl, "csr_scrakeyl")
  // BoringUtils.addSink(csr_scrakeyh, "csr_scrakeyh")
  // BoringUtils.addSink(csr_scrbkeyl, "csr_scrbkeyl")
  // BoringUtils.addSink(csr_scrbkeyh, "csr_scrbkeyh")

  // val pec_engine = Module(new Qarma.SingleCycle.QarmaEngine(max_round = 7))
  // pec_engine.input.bits.text := io.rs1
  // pec_engine.input.bits.tweak := io.rs2
  // pec_engine.input.bits.actual_round := 7.U(3.W)
  // pec_engine.input.bits.encrypt := ~io.insn(25)
  // pec_engine.output.ready := true.B
  // val key_sel = io.insn(14, 12)
  // pec_engine.input.valid := io.valid

  // pec_engine.input.bits.keyh := MuxLookup(key_sel, csr_scrtkeyh, Seq(
  //   "b000".U -> csr_scrtkeyh,
  //   "b001".U -> csr_mcrmkeyh,
  //   "b010".U -> csr_scrakeyh,
  //   "b011".U -> csr_scrbkeyh
  // ))
  // pec_engine.input.bits.keyl := MuxLookup(key_sel, csr_scrtkeyl, Seq(
  //   "b000".U -> csr_scrtkeyl,
  //   "b001".U -> csr_mcrmkeyl,
  //   "b010".U -> csr_scrakeyl,
  //   "b011".U -> csr_scrbkeyl
  // ))

  // io.rd := pec_engine.output.bits.result
  // io.done := pec_engine.output.valid
}
