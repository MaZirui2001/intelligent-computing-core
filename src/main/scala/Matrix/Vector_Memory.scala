import chisel3._
import chisel3.util._

class Vector_Memory_IO extends Bundle {
    val mem_type    = Input(UInt(2.W))
    val addr        = Input(UInt(32.W))
    val len         = Input(UInt(5.W))
    val data        = Input(Vec(32, UInt(32.W)))
    val res         = Output(UInt(32.W))
    val res_all     = Output(Vec(32, UInt(32.W)))
    val busy        = Output(Bool())
    val data_valid  = Output(Bool())

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
    val m_wlen      = Output(UInt(8.W))
    val m_bvalid    = Input(Bool())
    val m_bready    = Output(Bool())
}

class Vector_Memory extends Module {
    val io = IO(new Vector_Memory_IO)

    val busy            = WireDefault(false.B)

    val mem_type        = ShiftRegister(io.mem_type, 1, !busy)
    val addr            = ShiftRegister(io.addr, 1, !busy)
    val len             = ShiftRegister(io.len-1.U, 1, !busy)
    val data            = RegInit(VecInit.fill(32)(0.U(32.W)))
    val res_all         = RegInit(VecInit.fill(32)(0.U(32.W)))

    val m_rvalid        = WireDefault(false.B)
    val m_wvalid        = WireDefault(false.B)
    val m_bready        = WireDefault(false.B)
    val m_wlast         = WireDefault(false.B)
    val data_valid      = WireDefault(false.B)

    val wrt_cnt         = RegInit(0.U(8.W))
    val wrt_cnt_reset   = WireDefault(false.B)

    when(wrt_cnt_reset){
        wrt_cnt := io.len + 1.U
    }.elsewhen(io.m_wvalid && io.m_wready){
        wrt_cnt := wrt_cnt - 1.U
    }

    when(io.mem_type(1)){
        data := io.data
    }.elsewhen(io.m_wvalid && io.m_wready){
        data := (data.asUInt >> 32.U).asTypeOf(Vec(32, UInt(32.W)))
    }

    val rcnt = RegInit(0.U(5.W))
    when(io.mem_type.orR){
        rcnt := 0.U
        res_all := VecInit(Seq.fill(32)(0.U(32.W)))
    }
    .elsewhen(io.m_rvalid && io.m_rready){
        res_all(rcnt) := io.m_rdata
        rcnt := rcnt + 1.U
    }

    when(io.m_rvalid && io.m_rready){
        // res_all :=  ## (res_all.asUInt()).asTypeOf(Vec(32, UInt(32.W)))

    }
    
    val s_idle :: s_read :: s_write :: s_finish::Nil = Enum(4)
    val state = RegInit(s_idle)

    switch(state){
        is(s_idle){
            when(mem_type(0)){
                // read
                state       := s_read
            }.elsewhen(mem_type(1)){
                // write
                state           := s_write
                wrt_cnt_reset   := true.B
            }
            // busy           := io.mem_type.orR
            busy := mem_type.orR
        }
        is(s_read){
            // busy           := !(io.m_rlast && io.m_rready)
            busy           := true.B
            state          := Mux(io.m_rlast && io.m_rready, s_finish, s_read)
            m_rvalid        := true.B
            data_valid     := io.m_rvalid && io.m_rready
        }
        is(s_write){
            // busy           := !(io.m_bvalid && io.m_bready)
            busy           := true.B
            state          := Mux(io.m_bvalid && io.m_bready, s_finish, s_write)
            m_wvalid    := !wrt_cnt.andR
            m_bready    := true.B
            m_wvalid    := true.B
            m_wlast     := wrt_cnt === 0.U
        }
        is(s_finish){
            busy           := false.B
            state          := s_idle
        }
    }

    io.busy         := busy
    io.data_valid   := data_valid
    io.res          := io.m_rdata
    io.res_all      := res_all

    io.m_araddr     := addr
    io.m_rvalid     := m_rvalid
    io.m_rsize      := 2.U
    io.m_rburst     := 1.U
    io.m_rlen       := len

    io.m_awaddr     := addr
    io.m_wdata      := data(0)
    io.m_wvalid     := m_wvalid
    io.m_wlast      := m_wlast
    io.m_wstrb      := "b1111".U
    io.m_wsize      := 2.U
    io.m_wburst     := 1.U
    io.m_wlen       := len
    io.m_bready     := m_bready
}