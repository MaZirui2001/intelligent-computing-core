import chisel3._
import chisel3.util._
import Mat_Vec_Mul_Func._

class Mat_Vec_Mul_IO extends Bundle {
    // for pipeline
    val is_rvec     = Input(Bool())
    val is_mvmul    = Input(Bool())
    val stall       = Output(Bool())
    val vec_len     = Input(UInt(6.W))
    val mat_len     = Input(UInt(6.W))
    val mat_addr    = Input(UInt(32.W))
    val vec_addr    = Input(UInt(32.W))
    val dst_addr    = Input(UInt(32.W))

    // for axi
    val m_araddr    = Output(UInt(32.W))
    val m_rvalid    = Output(Bool())
    val m_rready    = Input(Bool())
    val m_rdata     = Input(UInt(32.W))
    val m_rlast     = Input(Bool())
    val m_rsize     = Output(UInt(2.W))
    val m_rburst    = Output(UInt(2.W))
    val m_rlen      = Output(UInt(8.W))

    val m_awaddr    = Output(UInt(32.W))
    val m_wdata     = Output(UInt(32.W))
    val m_wvalid    = Output(Bool())
    val m_wready    = Input(Bool())
    val m_wlast     = Output(Bool())
    val m_wstrb     = Output(UInt(4.W))
    val m_wsize     = Output(UInt(2.W))
    val m_wburst    = Output(UInt(2.W))
    val m_bvalid    = Input(Bool())
    val m_bready    = Output(Bool())
}

class Mat_Vec_Mul extends Module {
    val io = IO(new Mat_Vec_Mul_IO)

    val busy            = WireDefault(false.B)

    // request buffer
    val req_buf         = RegInit(0.U(110.W))
    val is_rvec         = req_buf(109)
    val is_mvmul        = req_buf(108)
    val vec_len         = req_buf(107, 102)
    val mat_len         = req_buf(101, 96)
    val mat_addr        = req_buf(95, 64)
    val vec_addr        = req_buf(63, 32)
    val dst_addr        = req_buf(31, 0)

    val config_set      = WireDefault(false.B)
    val cal_valid       = WireDefault(false.B)

    // vector buffer
    val vec_raddr       = RegInit(0.U(32.W))
    val vec_bottom      = RegInit(0.U(32.W))
    val vec_buf         = RegInit(VecInit(Seq.fill(64)(0.U(32.W))))
    val vec_buf_se      = WireDefault(false.B)

    // matrix buffer
    val mat_buf         = RegInit(0.U(32.W))
    val mat_buf_we      = WireDefault(false.B)

    // result buffer
    val res_buf         = RegInit(0.U(32.W))
    val res_add_time    = RegInit(0.U(32.W))
    val res_time_top    = RegInit(0.U(32.W))
    val sb_waddr        = RegInit(0.U(32.W))
    val sb_waddr_top    = RegInit(0.U(32.W))
    val result_sync     = WireDefault(false.B)

    // store buffer
    val store_buf       = RegInit(VecInit(Seq.fill(64)(0.U(32.W))))
    val store_buf_se    = WireDefault(false.B)


    /* request buffer */
    when(!busy){
        req_buf := Cat(io.is_rvec, io.is_mvmul, io.vec_len, io.mat_addr, io.mat_addr, io.vec_addr, io.dst_addr)
    }

    /* vector buffer */
    when(vec_buf_se){
        for(i <- 0 until 63){
            vec_buf(i) := vec_buf(i+1)
        }
        vec_buf(63) := io.m_rdata
    }
    when(config_set){
        vec_raddr   := 63.U - io.vec_len
        vec_bottom  := 63.U - io.vec_len
    }.elsewhen(cal_valid){
        vec_raddr   := Mux(vec_raddr === 63.U, vec_bottom, vec_raddr + 1.U)
    }
    val vector_elem_1 = vec_buf(vec_raddr)

    /* matrix buffer */
    when(mat_buf_we){
        mat_buf     := io.m_rdata
    }
    val matrix_elem_1 = mat_buf

    // stage 1-2 segreg
    val vector_elem_2 = ShiftRegister(vector_elem_1, 1)
    val matrix_elem_2 = ShiftRegister(matrix_elem_1, 1)
    val cal_valid_2   = ShiftRegister(cal_valid, 1)

    // stage 2: booth2
    val booth = VecInit(Seq.tabulate(32){ i => 
        Booth2(matrix_elem_2(31-2*i, 0) ## 0.U((2*i).W),
            (if(i == 0) vector_elem_2(1, 0) ## 0.U(1.W) else vector_elem_2(2*i+1, 2*i-1)), 32
        )
    })

    // stage 2-3 segreg
    val booth_3     = ShiftRegister(booth, 1)
    val cal_valid_3 = ShiftRegister(cal_valid_2, 1)

    // stage 3: CSA Tree
    val res = Wallce_Tree_32(booth_3)

    // stage 3-4 segreg
    val res_4       = ShiftRegister(res, 1)
    val cal_valid_4 = ShiftRegister(cal_valid_3, 1)

    when(config_set){
        res_time_top := io.vec_len
        res_add_time := 0.U
        sb_waddr_top := io.mat_len
        sb_waddr     := 0.U
    }.elsewhen(cal_valid_4){
        res_add_time := Mux(res_add_time === res_time_top, 0.U, res_add_time + 1.U)
        sb_waddr     := sb_waddr + 1.U
    }
    result_sync := (res_add_time === res_time_top) && cal_valid_4
    val result_elem = CSA(res_buf, res_4(0), res_4(1), 32)
    val result_temp = result_elem(31, 0) + result_elem(63, 32)

    /* result buffer */
    when(result_sync || config_set){
        res_buf := 0.U
    }.elsewhen(cal_valid_4){
        res_buf := result_temp
    }

    /* store buffer */
    when(result_sync){
        store_buf(sb_waddr) := result_temp
    }.elsewhen(config_set){
        for(i <- 0 until 63){
            store_buf(i) := store_buf(i+1)
        }
        store_buf(63) := 0.U
    }
    
}