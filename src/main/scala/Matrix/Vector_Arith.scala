import chisel3._
import chisel3.util._
import Control_Signal._

class Vector_Arith_IO extends Bundle {
    val src1    = Input(Vec(32, UInt(32.W)))
    val src2    = Input(Vec(32, UInt(32.W)))
    val en      = Input(Bool())
    val op      = Input(UInt(4.W))
    val busy    = Output(Bool())
    val res     = Output(Vec(32, UInt(32.W)))
}

class Vector_Arith extends Module{
    val io = IO(new Vector_Arith_IO)

    // 8 lane
    val src1    = ShiftRegister(io.src1, 1, !io.busy)
    val src2    = ShiftRegister(io.src2, 1, !io.busy)
    val op      = ShiftRegister(io.op, 1, !io.busy)

    // counter
    val cnt     = RegInit(4.U(8.W))
    when(!cnt(2)){
        cnt := cnt + 1.U
    }.elsewhen(io.en){
        cnt := 0.U
    }
    io.busy := !cnt(2)

    // 8 lane alu
    val res         = WireDefault(VecInit.fill(8)(0.U(32.W)))
    val alu_src1    = VecInit.tabulate(8)(i => src1(((cnt << 3.U) | i.U)(4, 0)))
    val alu_src2    = VecInit.tabulate(8)(i => src2(((cnt << 3.U) | i.U)(4, 0)))

    for(i <- 0 until 8){
        switch(op){
            is(ALU_ADD){
                res(i) := alu_src1(i) + alu_src2(i)
            }
            is(ALU_SUB){
                res(i) := alu_src1(i) - alu_src2(i)
            }
        }
    }

    // result buffer
    val res_reg = RegInit(VecInit.fill(4)(VecInit.fill(8)(0.U(32.W))))
    for(i <- 0 until 8){
        res_reg(cnt(1, 0)) := res
    }

    io.res := res_reg.asTypeOf(Vec(32, UInt(32.W)))

}
