import chisel3._
import chisel3.util._

class Vector_Unit_IO extends Bundle {
    val src1        = Input(Vec(32, UInt(32.W)))
    val src2        = Input(Vec(32, UInt(32.W)))
    val addr        = Input(UInt(32.W))
    val en          = Input(Bool())
    val is_mvmul    = Input(Bool())
    val mem_type    = Input(UInt(2.W))
    val vec_len     = Input(UInt(8.W))
    val mat_len     = Input(UInt(8.W))
    val op          = Input(UInt(4.W))
    val busy        = Output(Bool())
    val res         = Output(Vec(32, UInt(32.W)))

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

class Vector_Unit extends Module {
    val io = IO(new Vector_Unit_IO)
    val va = Module(new Vector_Arith)
    val vm = Module(new Vector_Memory)
    val mv = Module(new Mat_Vec_Mul)

    val is_arith = ShiftRegister(io.en && !io.is_mvmul && !io.mem_type.orR, 1, !io.busy)
    val is_mem   = ShiftRegister(io.en && !io.is_mvmul && io.mem_type.orR, 1, !io.busy)
    val is_mvmul = ShiftRegister(io.en && io.is_mvmul, 1, !io.busy)

    // vector arith
    va.io.src1  := io.src1
    va.io.src2  := io.src2
    va.io.en    := io.en && !io.is_mvmul && !io.mem_type.orR
    va.io.op    := io.op

    // vector memory
    vm.io.mem_type  := Mux(io.en, Mux(io.is_mvmul, 1.U, io.mem_type), 0.U)
    vm.io.addr      := io.addr
    vm.io.len       := io.vec_len
    vm.io.data      := io.src2
    vm.io.m_rready  := io.m_rready
    vm.io.m_rdata   := io.m_rdata
    vm.io.m_rlast   := io.m_rlast
    vm.io.m_wready  := io.m_wready
    vm.io.m_bvalid  := io.m_bvalid

    // mat vec mul
    mv.io.en            := io.en && io.is_mvmul
    mv.io.vec_len       := io.vec_len
    mv.io.mat_len       := io.mat_len
    mv.io.mat_addr      := io.addr
    mv.io.vec_data      := io.src2
    mv.io.data_valid    := vm.io.data_valid
    mv.io.rdata         := vm.io.res

    // busy
    io.busy := va.io.busy || vm.io.busy || mv.io.busy

    // result
    io.res := Mux1H(Seq(
        is_arith -> va.io.res,
        is_mem   -> vm.io.res_all,
        is_mvmul -> mv.io.res
    ))

    io.m_araddr     := vm.io.m_araddr
    io.m_rvalid     := vm.io.m_rvalid
    io.m_rsize      := vm.io.m_rsize
    io.m_rburst     := vm.io.m_rburst
    io.m_rlen       := vm.io.m_rlen

    io.m_awaddr     := vm.io.m_awaddr
    io.m_wdata      := vm.io.m_wdata
    io.m_wvalid     := vm.io.m_wvalid
    io.m_wlast      := vm.io.m_wlast
    io.m_wstrb      := vm.io.m_wstrb
    io.m_wsize      := vm.io.m_wsize
    io.m_wburst     := vm.io.m_wburst
    io.m_wlen       := vm.io.m_wlen
    io.m_bready     := vm.io.m_bready


}