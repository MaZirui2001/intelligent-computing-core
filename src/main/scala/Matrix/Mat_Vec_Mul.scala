import chisel3._
import chisel3.util._
import Mat_Vec_Mul_Func._
import dataclass.data

class Mat_Vec_Mul_IO extends Bundle {
    // for pipeline
    val en          = Input(Bool())
    val vec_len     = Input(UInt(6.W))
    val mat_len     = Input(UInt(6.W))
    val mat_addr    = Input(UInt(32.W))
    val vec_data    = Input(Vec(32, UInt(32.W)))
    val busy        = Output(Bool())
    val res         = Output(Vec(32, UInt(32.W)))

    // for axi
    val data_valid  = Input(Bool())
    val rdata       = Input(UInt(32.W))
}

class Mat_Vec_Mul extends Module {
    val io = IO(new Mat_Vec_Mul_IO)

    val busy            = WireDefault(false.B)

    // request buffer
    val vec_len         = ShiftRegister(io.vec_len, 1, !busy && io.en)
    val mat_len         = ShiftRegister(io.mat_len+1.U, 1, 0.U, !busy && io.en)
    val mat_addr        = ShiftRegister(io.mat_addr, 1, !busy && io.en)
    // val vec_data        = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val en              = ShiftRegister(io.en, 1, !busy)
    
    val config_set      = io.en
    val cal_valid       = io.data_valid && en

    // vector buffer
    val vec_raddr       = RegInit(0.U(5.W))
    val vec_buf         = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

    // matrix buffer
    val mat_buf         = RegInit(0.U(32.W))
    val mat_buf_we      = WireDefault(false.B)

    // result buffer
    val res_buf         = RegInit(0.U(32.W))
    val res_add_time    = RegInit(0.U(32.W))
    val sb_waddr        = RegInit(0.U(5.W))
    val result_sync     = WireDefault(false.B)

    // store buffer
    val store_buf       = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val store_buf_se    = WireDefault(false.B)


    /* vector buffer */
    when(io.en){
        vec_buf := io.vec_data
    }

    when(config_set){
        vec_raddr   := 0.U
    }.elsewhen(cal_valid){
        vec_raddr   := Mux(vec_raddr === vec_len,  0.U, vec_raddr + 1.U)
    }
    val vector_elem_1 = vec_buf(vec_raddr)

    /* matrix buffer */
    // when(io.data_valid){
    //     mat_buf     := io.rdata
    // }
    val matrix_elem_1 = io.rdata

    // stage 1-2 segreg
    val vector_elem_2 = ShiftRegister(vector_elem_1, 1)
    val matrix_elem_2 = ShiftRegister(matrix_elem_1, 1)
    val cal_valid_2   = ShiftRegister(cal_valid, 1)

    // stage 2: booth2
    val booth = VecInit(Seq.tabulate(16){ i => 
        Booth2(matrix_elem_2(31-2*i, 0) ## 0.U((2*i).W),
            (if(i == 0) vector_elem_2(1, 0) ## 0.U(1.W) else vector_elem_2(2*i+1, 2*i-1)), 32
        )
    })

    // stage 2-3 segreg
    val booth_3     = ShiftRegister(booth, 1)
    val cal_valid_3 = ShiftRegister(cal_valid_2, 1)

    // stage 3: CSA Tree
    val res = Wallce_Tree_16(booth_3)

    // stage 3-4 segreg
    val res_4       = ShiftRegister(res, 1)
    val cal_valid_4 = ShiftRegister(cal_valid_3, 1)

    when(config_set){
        res_add_time := 0.U
        sb_waddr     := 0.U
    }.elsewhen(cal_valid_4){
        res_add_time := Mux(res_add_time === vec_len, 0.U, res_add_time + 1.U)
        // sb_waddr     := sb_waddr + 1.U
    }
    result_sync := (res_add_time === vec_len) && cal_valid_4
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
        sb_waddr := sb_waddr + 1.U
    }

    busy    := (sb_waddr =/= mat_len) && en
    io.busy := busy
    io.res  := store_buf

    
}