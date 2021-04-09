// See LICENSE.SiFive for license details.

package freechips.rocketchip.scie

import chisel3._
import chisel3.util._

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
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
}

class SCIEUnpipelined(xLen: Int) extends Module {
  val io = IO(new SCIEUnpipelinedInterface(xLen))

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

  val pec_engine = Module(new Qarma.SingleCycle.QarmaEngine(max_round = 7))
  pec_engine.input.bits.text := io.rs1
  pec_engine.input.bits.tweak := io.rs2
  pec_engine.input.bits.actual_round := 7.U(3.W)
  pec_engine.input.bits.encrypt := ~io.insn(25)
  pec_engine.output.ready := true.B
  val key_sel = io.insn(14, 12)
  pec_engine.input.valid := true.B

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

  io.rd := pec_engine.output.bits.result
}

class SCIEPipelinedInterface(xLen: Int) extends Bundle {
  val clock = Input(Clock())
  val valid = Input(Bool())
  val insn = Input(UInt(SCIE.iLen.W))
  val rs1 = Input(UInt(xLen.W))
  val rs2 = Input(UInt(xLen.W))
  val rd = Output(UInt(xLen.W))
}

class SCIEPipelined(xLen: Int) extends BlackBox(Map("XLEN" -> xLen)) with HasBlackBoxInline {
  val io = IO(new SCIEPipelinedInterface(xLen))

  setInline("SCIEPipelined.v",
    s"""
      |module SCIEPipelined #(parameter XLEN = 32) (
      |    input clock,
      |    input valid,
      |    input [${SCIE.iLen-1}:0] insn,
      |    input [XLEN-1:0] rs1,
      |    input [XLEN-1:0] rs2,
      |    output [XLEN-1:0] rd);
      |
      |  /* This example SCIE implementation provides the following instructions:
      |
      |     Major opcode custom-0:
      |     Funct3 = 2: AD.U8, compute absolute differences of packed uint8
      |       rd[7:0] = abs(rs1[7:0] - rs2[7:0])
      |       rd[15:8] = abs(rs1[15:8] - rs2[15:8])
      |       ...
      |       rd[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
      |
      |     Funct3 = 3: SAD.U8, compute sum of absolute differences of packed uint8
      |       tmp[7:0] = abs(rs1[7:0] - rs2[7:0])
      |       tmp[15:8] = abs(rs1[15:8] - rs2[15:8])
      |       ...
      |       tmp[XLEN-1:XLEN-8] = abs(rs1[XLEN-1:XLEN-8] - rs2[XLEN-1:XLEN-8])
      |
      |       rd = tmp[7:0] + tmp[15:8] + ... + tmp[XLEN-1:XLEN-8]
      |  */
      |
      |  integer i;
      |  reg [XLEN-1:0] absolute_differences;
      |  reg funct3_0;
      |  reg [XLEN-1:0] result;
      |
      |`ifndef RANDOM
      |`define RANDOM $$random
      |`endif
      |
      |  always @(posedge clock)
      |  begin
      |    /* Gating using the valid signal is optional, but saves power. */
      |    if (valid)
      |    begin
      |      /* Register Funct3[0] opcode bit for result muxing in next stage. */
      |      funct3_0 <= insn[12];
      |
      |      /* Compute each absolute difference and register each result. */
      |      for (i = 0; i < XLEN/8; i = i + 1)
      |      begin
      |        absolute_differences[8*i +: 8] <= rs1[8*i +: 8] < rs2[8*i +: 8] ?
      |                                          rs2[8*i +: 8] - rs1[8*i +: 8] :
      |                                          rs1[8*i +: 8] - rs2[8*i +: 8];
      |      end
      |    end
      |  end
      |
      |  /* In the second pipeline stage, compute the final result. */
      |  always @(*)
      |  begin
      |    if (!funct3_0)
      |    begin
      |      /* If Funct3[0] = 0, the output is the packed absolute differences. */
      |      result = absolute_differences;
      |    end
      |    else
      |    begin
      |      /* If Funct3[0] = 1, the output is their sum. */
      |      result = {XLEN{1'b0}};
      |      for (i = 0; i < XLEN/8; i = i + 1)
      |      begin
      |        result = result + {{(XLEN-8){1'b0}}, absolute_differences[8*i +: 8]};
      |      end
      |    end
      |  end
      |
      |  /* Drive the output. */
      |  assign rd = result;
      |
      | /* Suppress Xs at simulation start */
      | `ifdef RANDOMIZE_REG_INIT
      | initial begin
      |   `ifndef VERILATOR
      |   #`RANDOMIZE_DELAY begin end
      |   `endif
      |   absolute_differences = {(XLEN / 32){`RANDOM}};
      |   funct3_0 = absolute_differences[0];
      | end
      | `endif
      |
      |endmodule
     """.stripMargin)
}
