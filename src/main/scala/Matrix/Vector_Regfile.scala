import chisel3._
import chisel3.util._


class Vector_Regfile_IO extends Bundle {

    val raddr1 = Input(UInt(3.W))
    val raddr2 = Input(UInt(3.W))
    val rdata1 = Output(Vec(32, UInt(32.W)))
    val rdata2 = Output(Vec(32, UInt(32.W)))

    val waddr  = Input(UInt(3.W))
    val wdata  = Input(Vec(32, UInt(32.W)))
    val we     = Input(Bool())
}
class Vector_Regfile extends Module{
    val io = IO(new Vector_Regfile_IO)
    val vrf = RegInit(VecInit.fill(8)(VecInit.fill(32)(0.U(32.W))))
    io.rdata1 := Mux(io.we && io.waddr === io.raddr1, io.wdata, vrf(io.raddr1))
    io.rdata2 := Mux(io.we && io.waddr === io.raddr2, io.wdata, vrf(io.raddr2))

    when(io.we){
        vrf(io.waddr) := io.wdata
    }
}