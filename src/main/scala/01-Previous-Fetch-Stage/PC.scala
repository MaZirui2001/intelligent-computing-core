import chisel3._
import chisel3.util._

class PC_IO extends Bundle {
    val pc_PF           = Output(UInt(32.W))
    val pc_stall        = Input(Bool())
    val predict_fail    = Input(Bool())
    val npc             = Output(UInt(32.W))
    val pred_jump       = Input(Vec(2, Bool()))
    val pred_npc        = Input(UInt(32.W))
    val branch_target   = Input(UInt(32.W))
    val inst_valid_PF   = Output(Vec(2, Bool()))

    val flush_by_pd     = Input(Bool())
    val flush_pd_target = Input(UInt(32.W))
}

class PC(reset_val: Int) extends Module {
    val io = IO(new PC_IO)

    val pc = RegInit(reset_val.U(32.W))

    when(io.predict_fail) {
        io.npc := io.branch_target
    }
    .elsewhen(io.flush_by_pd){
        io.npc := io.flush_pd_target
    }
    .elsewhen(!io.pc_stall) {
        when(io.pred_jump.reduce(_||_)){
            io.npc := io.pred_npc
        }.otherwise{
            io.npc := (pc + 8.U)(31, 3) ## 0.U(3.W)
        }
    }
    .otherwise{
        io.npc := pc
    }

    io.pc_PF := pc
    pc := io.npc

    io.inst_valid_PF(0) := true.B
    io.inst_valid_PF(1) := !io.pred_jump(0) && !pc(2) 
    //val inst_valid_temp = VecInit(PriorityEncoderOH(io.pred_jump))

    //io.inst_valid_PF := (((inst_valid_temp.asUInt(2, 0) ## 0.U(1.W)) - 1.U(4.W)) & (15.U(4.W) >> pc(3, 2))).asBools

}