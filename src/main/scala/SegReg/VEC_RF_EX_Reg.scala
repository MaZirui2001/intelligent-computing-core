import chisel3._
import chisel3.util._
import Inst_Pack._

class VEC_RF_EX_Reg[T <: Bundle](inst_pack_t: T) extends Module {
    val io = IO(new Bundle {
        val flush           = Input(Bool())
        val stall           = Input(Bool())
        val inst_pack_RF    = Input(inst_pack_t)
        val src1_RF         = Input(Vec(32, UInt(32.W)))
        val src2_RF         = Input(Vec(32, UInt(32.W)))
        val addr_RF         = Input(UInt(32.W))

        val inst_pack_EX    = Output(inst_pack_t)
        val src1_EX         = Output(Vec(32, UInt(32.W)))
        val src2_EX         = Output(Vec(32, UInt(32.W)))
        val addr_EX         = Output(UInt(32.W))

    })

    val inst_pack_reg   = RegInit(0.U.asTypeOf(inst_pack_t))
    val src1_reg        = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val src2_reg        = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val addr_reg        = RegInit(0.U(32.W))

    when(io.flush){
        inst_pack_reg   := 0.U.asTypeOf(inst_pack_t)
    }.elsewhen(!io.stall){
        inst_pack_reg   := io.inst_pack_RF
        src1_reg        := io.src1_RF
        src2_reg        := io.src2_RF
        addr_reg        := io.addr_RF

    }

    io.inst_pack_EX := inst_pack_reg
    io.src1_EX      := src1_reg
    io.src2_EX      := src2_reg
    io.addr_EX      := addr_reg
}


