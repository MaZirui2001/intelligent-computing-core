
import chisel3._
import chisel3.util._
import Inst_Pack._

class VEC_EX_WB_Reg extends Module {
    val io = IO(new Bundle {
        val flush           = Input(Bool())
        val stall           = Input(Bool())
        val inst_pack_EX    = Input(new inst_pack_IS_VEC_t)
        val vec_res_EX      = Input(Vec(32, UInt(32.W)))
    
        val inst_pack_WB    = Output(new inst_pack_IS_VEC_t)
        val vec_res_WB      = Output(Vec(32, UInt(32.W)))
    })  

    val inst_pack_reg   = RegInit(0.U.asTypeOf(new inst_pack_IS_VEC_t))
    val vec_res_reg     = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

    when(io.flush){
        inst_pack_reg   := 0.U.asTypeOf(new inst_pack_IS_VEC_t)
    }.elsewhen(!io.stall){
        inst_pack_reg   := io.inst_pack_EX
        vec_res_reg     := io.vec_res_EX
    }
    io.inst_pack_WB     := inst_pack_reg
    io.vec_res_WB       := vec_res_reg
}