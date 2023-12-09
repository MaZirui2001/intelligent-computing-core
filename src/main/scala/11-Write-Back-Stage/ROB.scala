import chisel3._
import chisel3.util._
import PRED_Config._
import CPU_Config._

object ROB_Pack{
    class rob_t extends Bundle(){
        val rd                  = UInt(5.W)
        val rd_valid            = Bool()
        val prd                 = UInt(log2Ceil(PREG_NUM).W)
        val pprd                = UInt(log2Ceil(PREG_NUM).W)
        val predict_fail        = Bool()
        val branch_target       = UInt(32.W)
        val real_jump           = Bool()
        val pred_update_en      = Bool()
        val br_type_pred        = UInt(2.W)
        val complete            = Bool()
        val pc                  = UInt(30.W)
        val rf_wdata            = UInt(32.W)
        val is_store            = Bool()
        val is_ucread           = Bool()
        val is_priv_wrt         = Bool() 
    }
    class priv_t(n: Int) extends Bundle{
        val valid = Bool()
        val rob_index = UInt(log2Ceil(n).W)
        val priv_vec = UInt(4.W)
        val csr_addr = UInt(14.W)
        val csr_wdata = UInt(32.W)
    }
}
class ROB_IO(n: Int) extends Bundle{
    // for reg rename
    val inst_valid_dp           = Input(Vec(2, Bool()))
    val rd_dp                   = Input(Vec(2, UInt(5.W)))
    val rd_valid_dp             = Input(Vec(2, Bool()))
    val prd_dp                  = Input(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val pprd_dp                 = Input(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val rob_index_dp            = Output(Vec(2, UInt(log2Ceil(n).W)))
    val pc_dp                   = Input(Vec(2, UInt(32.W)))
    val is_store_dp             = Input(Vec(2, Bool()))
    val br_type_pred_dp         = Input(Vec(2, UInt(2.W)))
    val pred_update_en_dp       = Input(Vec(2, Bool()))
    val csr_addr_dp             = Input(Vec(2, UInt(14.W)))
    val priv_vec_dp             = Input(Vec(2, UInt(4.W)))
    val full                    = Output(Bool())
    val stall                   = Input(Bool())

    // for wb stage 
    val inst_valid_wb           = Input(Vec(5, Bool()))
    val rob_index_wb            = Input(Vec(5, UInt(log2Ceil(n).W)))
    val is_ucread_wb            = Input(Vec(5, Bool()))
    val predict_fail_wb         = Input(Vec(5, Bool()))
    val real_jump_wb            = Input(Vec(5, Bool()))
    val branch_target_wb        = Input(Vec(5, UInt(32.W)))
    val rf_wdata_wb             = Input(Vec(5, UInt(32.W)))

    // for cpu state: arch rat
    val cmt_en                  = Output(Vec(2, Bool()))

    val prd_cmt                 = Output(Vec(2, UInt(log2Ceil(PREG_NUM).W)))
    val rd_valid_cmt            = Output(Vec(2, Bool()))
    val pprd_cmt                = Output(Vec(2, UInt(log2Ceil(PREG_NUM).W)))

    // for store buffer
    val is_store_num_cmt        = Output(UInt(2.W))

    // for predict and ras
    val predict_fail_cmt        = Output(UInt(10.W)) // opt fanout
    val pred_update_en_cmt      = Output(Bool())
    val pred_branch_target_cmt  = Output(UInt(32.W))
    val pred_pc_cmt             = Output(UInt(32.W))
    val pred_real_jump_cmt      = Output(Bool())
    val br_type_pred_cmt        = Output(UInt(2.W))

    // for csr write
    val csr_addr_cmt            = Output(UInt(14.W))
    val csr_wdata_cmt           = Output(UInt(32.W))
    val csr_we_cmt              = Output(Bool())

    // diff
    val is_ucread_cmt           = Output(Vec(2, Bool()))
    val rd_cmt                  = Output(Vec(2, UInt(5.W)))
    val rf_wdata_cmt            = Output(Vec(2, UInt(32.W)))
    val branch_target_cmt       = Output(UInt(32.W))
    val pc_cmt                  = Output(Vec(2, UInt(32.W)))
    val csr_diff_addr_cmt       = Output(Vec(2, UInt(14.W)))
    val csr_diff_wdata_cmt      = Output(Vec(2, UInt(32.W)))
    val csr_diff_we_cmt         = Output(Vec(2, Bool()))

    // stat
    val predict_fail_stat       = Output(Vec(2, Bool()))
    val br_type_stat            = Output(Vec(2, UInt(2.W)))
    val is_br_stat              = Output(Vec(2, Bool()))
}

class ROB(n: Int) extends Module{
    val io = IO(new ROB_IO(n))
    val neach = n / 2
    import ROB_Pack._
    /* ROB items */
    val rob         = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(neach)(0.U.asTypeOf(new rob_t))))))
    val priv_buf    = RegInit(0.U.asTypeOf(new priv_t(n)))

    /* ROB ptrs */
    val head        = RegInit(0.U(log2Ceil(n).W))
    val head_each   = VecInit(Seq.tabulate(2)(i => head + i.U(log2Ceil(n).W)))
    val tail        = RegInit(0.U(log2Ceil(neach).W))
    val elem_num    = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(neach)+1).W))))
    val hsel_idx    = VecInit.tabulate(2)(i => head_each(i)(0))
    val head_idx    = VecInit.tabulate(2)(i => head_each(i)(log2Ceil(n)-1, 1))

    /* ROB status */
    val empty       = VecInit(elem_num.map(_ === 0.U))
    val full        = VecInit(elem_num.map(_ === neach.U)).reduce(_||_)

    val inst_valid_dp = io.inst_valid_dp(0)
    // rn stage
    when(!full){
        for(i <- 0 until 2){
            when(inst_valid_dp){
                rob(i)(tail).rd              := io.rd_dp(i)
                rob(i)(tail).rd_valid        := io.rd_valid_dp(i)
                rob(i)(tail).prd             := io.prd_dp(i)
                rob(i)(tail).pprd            := io.pprd_dp(i)
                rob(i)(tail).pc              := io.pc_dp(i)(31, 2)
                rob(i)(tail).is_store        := io.is_store_dp(i)
                rob(i)(tail).br_type_pred    := io.br_type_pred_dp(i)
                rob(i)(tail).pred_update_en  := io.pred_update_en_dp(i)
                rob(i)(tail).complete        := false.B
                rob(i)(tail).is_priv_wrt     := io.priv_vec_dp(i)(0) && io.priv_vec_dp(i)(3, 1).orR
            }
        }
        val priv_bits = VecInit.tabulate(2)(i => io.priv_vec_dp(i)(0) && io.priv_vec_dp(i)(3, 1).orR)
        val priv_index = PriorityEncoder(priv_bits)
        when(!priv_buf.valid && inst_valid_dp && priv_bits.reduce(_||_)){
            priv_buf.csr_addr  := io.csr_addr_dp(priv_index)
            priv_buf.priv_vec  := io.priv_vec_dp(priv_index)
            priv_buf.valid     := true.B
            priv_buf.rob_index := tail ## priv_index
        }

    }
    io.rob_index_dp := VecInit.tabulate(2)(i => tail ## i.U(1.W))
    io.full := full
    // wb stage
    for(i <- 0 until 5){
        when(io.inst_valid_wb(i)){
            val col_idx = io.rob_index_wb(i)(0)
            val row_idx = io.rob_index_wb(i)(log2Ceil(n)-1, 1)
            rob(col_idx)(row_idx).complete        := true.B
            rob(col_idx)(row_idx).predict_fail    := io.predict_fail_wb(i)
            rob(col_idx)(row_idx).branch_target   := io.branch_target_wb(i)
            rob(col_idx)(row_idx).rf_wdata        := io.rf_wdata_wb(i)
            rob(col_idx)(row_idx).real_jump       := io.real_jump_wb(i)
            rob(col_idx)(row_idx).is_ucread       := io.is_ucread_wb(i)
        }
    }
    when(io.inst_valid_wb(1) && io.rob_index_wb(1) === priv_buf.rob_index){
        priv_buf.csr_wdata := io.branch_target_wb(1)
    }
    
    // cmt stage
    val cmt_en    = Wire(Vec(2, Bool()))
    cmt_en(0) := rob(hsel_idx(0))(head_idx(0)).complete && !empty(hsel_idx(0))
    for(i <- 1 until 2){
        cmt_en(i) := (cmt_en(i-1) && rob(hsel_idx(i))(head_idx(i)).complete 
                        && !rob(hsel_idx(i-1))(head_idx(i-1)).pred_update_en 
                        && !rob(hsel_idx(i-1))(head_idx(i-1)).is_priv_wrt
                        && !empty(hsel_idx(i)))
    }
    io.cmt_en := ShiftRegister(cmt_en, 1, VecInit(Seq.fill(2)(false.B)), true.B)
    

    // update predict and ras
    val update_ptr              = head + PopCount(cmt_en) - 1.U
    val rob_update_item         = Mux(cmt_en(0) === false.B, 0.U.asTypeOf(new rob_t), rob(update_ptr(0))(update_ptr(log2Ceil(n)-1, 1)))
    

    val predict_fail_cmt         = rob_update_item.predict_fail || rob_update_item.is_priv_wrt
    val branch_target_cmt        = Mux(rob_update_item.is_priv_wrt || !rob_update_item.real_jump, (rob_update_item.pc ## 0.U(2.W)) + 4.U, rob_update_item.branch_target)
    val pred_update_en_cmt       = rob_update_item.pred_update_en
    val pred_branch_target_cmt   = rob_update_item.branch_target
    val br_type_pred_cmt         = rob_update_item.br_type_pred
    val pred_pc_cmt              = rob_update_item.pc ## 0.U(2.W)
    val pred_real_jump_cmt       = rob_update_item.real_jump

    io.predict_fail_cmt         := ShiftRegister(VecInit.fill(10)(predict_fail_cmt).asUInt, 1, 0.U(10.W), true.B)
    io.branch_target_cmt        := ShiftRegister(branch_target_cmt, 1, 0.U(32.W), true.B)
    io.pred_update_en_cmt       := ShiftRegister(pred_update_en_cmt, 1, false.B, true.B)
    io.pred_branch_target_cmt   := ShiftRegister(pred_branch_target_cmt, 1, 0.U(32.W), true.B)
    io.br_type_pred_cmt         := ShiftRegister(br_type_pred_cmt, 1, 0.U(2.W), true.B)
    io.pred_pc_cmt              := ShiftRegister(pred_pc_cmt, 1, 0x1c000000.U, true.B)
    io.pred_real_jump_cmt       := ShiftRegister(pred_real_jump_cmt, 1, false.B, true.B)


    // update store buffer
    val rob_commit_items        = VecInit.tabulate(2)(i => rob(hsel_idx(i))(head_idx(i)))
    val is_store_cmt_bit        = VecInit.tabulate(2)(i => rob_commit_items(i).is_store && cmt_en(i))
    val is_store_num_cmt        = PopCount(is_store_cmt_bit)
    io.is_store_num_cmt         := ShiftRegister(is_store_num_cmt, 1, 0.U(2.W), true.B)

    // update csr file
    val csr_addr_cmt            = priv_buf.csr_addr
    val csr_wdata_cmt           = priv_buf.csr_wdata
    val csr_we_cmt              = rob_update_item.is_priv_wrt && priv_buf.priv_vec(2, 1).orR

    io.csr_addr_cmt             := ShiftRegister(csr_addr_cmt, 1, 0.U, true.B)
    io.csr_wdata_cmt            := ShiftRegister(csr_wdata_cmt, 1, 0.U, true.B)
    io.csr_we_cmt               := ShiftRegister(csr_we_cmt, 1, false.B, true.B)
    when(io.predict_fail_cmt(0)){
        priv_buf.valid          := false.B
    }
    
    val rd_cmt                   = VecInit.tabulate(2)(i => rob_commit_items(i).rd)
    val rd_valid_cmt             = VecInit.tabulate(2)(i => rob_commit_items(i).rd_valid)
    val prd_cmt                  = VecInit.tabulate(2)(i => rob_commit_items(i).prd)
    val pprd_cmt                 = VecInit.tabulate(2)(i => rob_commit_items(i).pprd)
    val pc_cmt                   = VecInit.tabulate(2)(i => Mux(rob_commit_items(i).real_jump, rob_commit_items(i).branch_target, (rob_commit_items(i).pc ## 0.U(2.W)) + 4.U))
    val rf_wdata_cmt             = VecInit.tabulate(2)(i => rob_commit_items(i).rf_wdata)
    val is_ucread_cmt            = VecInit.tabulate(2)(i => rob_commit_items(i).is_ucread && cmt_en(i))
    val csr_diff_addr_cmt        = VecInit.fill(2)(priv_buf.csr_addr)
    val csr_diff_wdata_cmt       = VecInit.fill(2)(priv_buf.csr_wdata)
    val csr_diff_we_cmt          = VecInit.tabulate(2)(i => Mux(rob_commit_items(i).is_priv_wrt, priv_buf.priv_vec(2, 1).orR, false.B))

    io.rd_valid_cmt             := ShiftRegister(rd_valid_cmt, 1, VecInit(Seq.fill(2)(false.B)), true.B)
    io.rd_cmt                   := ShiftRegister(rd_cmt, 1, VecInit(Seq.fill(2)(0.U(5.W))), true.B)
    io.prd_cmt                  := ShiftRegister(prd_cmt, 1, VecInit(Seq.fill(2)(0.U(log2Ceil(PREG_NUM).W))), true.B)
    io.pprd_cmt                 := ShiftRegister(pprd_cmt, 1, VecInit(Seq.fill(2)(0.U(log2Ceil(PREG_NUM).W))), true.B)
    io.pc_cmt                   := ShiftRegister(pc_cmt, 1, VecInit(Seq.fill(2)(0.U(32.W))), true.B)
    io.rf_wdata_cmt             := ShiftRegister(rf_wdata_cmt, 1, VecInit(Seq.fill(2)(0.U(32.W))), true.B)
    io.is_ucread_cmt            := ShiftRegister(is_ucread_cmt, 1, VecInit(Seq.fill(2)(false.B)), true.B)
    io.csr_diff_addr_cmt        := ShiftRegister(csr_diff_addr_cmt, 1, VecInit(Seq.fill(2)(0.U(32.W))), true.B)
    io.csr_diff_wdata_cmt       := ShiftRegister(csr_diff_wdata_cmt, 1, VecInit(Seq.fill(2)(0.U(32.W))), true.B)
    io.csr_diff_we_cmt          := ShiftRegister(csr_diff_we_cmt, 1, VecInit(Seq.fill(2)(false.B)), true.B)

    
    // update ptrs
    val cmt_num                 = PopCount(cmt_en)
    head                        := Mux(io.predict_fail_cmt(0) || predict_fail_cmt, 0.U, Mux(head + cmt_num >= n.U, head + cmt_num - n.U, head + cmt_num))                 
    val head_inc                = VecInit(Seq.fill(2)(false.B))
    for(i <- 0 until 2){
        head_inc(hsel_idx(i))   := cmt_en(i)
        elem_num(i)             := Mux(io.predict_fail_cmt(0) || predict_fail_cmt, 0.U, Mux(!full && !io.stall, elem_num(i) + inst_valid_dp - head_inc(i), elem_num(i) - head_inc(i)))
    }
    tail := Mux(io.predict_fail_cmt(0) || predict_fail_cmt, 0.U, Mux(!full && !io.stall, Mux(tail + inst_valid_dp === neach.U, 0.U, tail + inst_valid_dp), tail))


    // stat
    io.predict_fail_stat        := ShiftRegister(VecInit.tabulate(2)(i => rob(hsel_idx(i))(head_idx(i)).predict_fail & cmt_en(i)), 1, VecInit(Seq.fill(2)(false.B)), true.B)
    io.br_type_stat             := ShiftRegister(VecInit.tabulate(2)(i => rob(hsel_idx(i))(head_idx(i)).br_type_pred), 1, VecInit(Seq.fill(2)(0.U(2.W))), true.B)
    io.is_br_stat               := ShiftRegister(VecInit.tabulate(2)(i => rob(hsel_idx(i))(head_idx(i)).pred_update_en & cmt_en(i)), 1, VecInit(Seq.fill(2)(false.B)), true.B)
} 