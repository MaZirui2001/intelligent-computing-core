import chisel3._
import chisel3.util._

class Vector_Multiply_IO extends Bundle {
    val vec1  = Input(Vec(32, UInt(32.W)))
    val vec2  = Input(Vec(32, UInt(32.W)))
    val en    = Input(Bool())
    val res   = Output(UInt(32.W))
}
class Vector_Multiply extends Module{
    val io = IO(new Vector_Multiply_IO)
    def Booth2(x: UInt, y: UInt, n: Int) :UInt = {
        val res = WireDefault(x)
        val neg = y(2)
        switch(y){
            is(0.U){ res := 0.U }
            is(3.U){ res := x(n-2, 0) ## 0.U(1.W) }
            is(4.U){ res := x(n-2, 0) ## 0.U(1.W) }
            is(7.U){ res := 0.U }
        }
        Mux(neg, ~res + 1.U, res)
    }
    def CSA(x: UInt, y: UInt, z: UInt, n: Int) : UInt  = {
        val x_xor_y = WireDefault(x ^ y)
        val res1 = WireDefault(x_xor_y ^ z)
        val res2 = WireDefault((((x & y) | z & (x_xor_y)) ## 0.U(1.W))(n-1, 0))
        res2 ## res1
    }

    // stage1: booth for each element
    val vec1_reg_1 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val vec2_reg_1 = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    val en_reg_1   = ShiftRegister(io.en, 1)

    for(i <- 0 until 32){
        vec1_reg_1(i) := io.vec1(i)
        vec2_reg_1(i) := io.vec2(i)
    }
    val booth = VecInit.tabulate(32){
        i => VecInit.tabulate(16){
            j => (
                Booth2(vec1_reg_1(i)(32-2*j, 0) ## 0.U((2*j).W), 
                (if (j == 0) vec2_reg_1(i)(1, 0) ## 0.U(1.W) else vec2_reg_1(i)(2*j+1, 2*j-1)), 32)
            )
        }
    }
    // stage2: CSA Tree level 1-5
    val vec_reg_2   = RegInit(VecInit(Seq.fill(32)(VecInit(Seq.fill(17)(0.U(32.W))))))
    val en_reg_2    = ShiftRegister(en_reg_1, 1)
    vec_reg_2       := booth
    val src = Wire(Vec(32*17, UInt(32.W))) // 544
    for(i <- 0 until 32){
        for(j <- 0 until 17){
            src(i*17+j) := vec_reg_2(i)(j)
        }
    }

    // level1: input 544, output 363
    val level1        = VecInit(Seq.tabulate(181){i => CSA(src(3*i), src(3*i+1), src(3*i+2), 32)})
    val res1          = Wire(Vec(363, UInt(32.W)))
    for(i <- 0 until 181){
        res1(2*i)      := level1(i)(31, 0)
        res1(2*i+1)    := level1(i)(63, 32)
    }
    res1(362)         := src(543)
    // level2: input 363, output 242
    val level2        = VecInit(Seq.tabulate(121){i => CSA(res1(3*i), res1(3*i+1), res1(3*i+2), 32)})
    val res2          = Wire(Vec(242, UInt(32.W)))
    for(i <- 0 until 121){
        res2(2*i)      := level2(i)(31, 0)
        res2(2*i+1)    := level2(i)(63, 32)
    }
    res2(241)         := res1(362)
    // level3: input 242, output 162
    val level3        = VecInit(Seq.tabulate(80){i => CSA(res2(3*i), res2(3*i+1), res2(3*i+2), 32)})
    val res3          = Wire(Vec(162, UInt(32.W)))
    for(i <- 0 until 80){
        res3(2*i)      := level3(i)(31, 0)
        res3(2*i+1)    := level3(i)(63, 32)
    }
    res3(160)         := res2(240)
    res3(161)         := res2(241)
    // level4: input 162, output 108
    val level4        = VecInit(Seq.tabulate(54){i => CSA(res3(3*i), res3(3*i+1), res3(3*i+2), 32)})
    val res4          = Wire(Vec(108, UInt(32.W)))
    for(i <- 0 until 54){
        res4(2*i)      := level4(i)(31, 0)
        res4(2*i+1)    := level4(i)(63, 32)
    }
    // level5: input 108, output 72
    val level5        = VecInit(Seq.tabulate(36){i => CSA(res4(3*i), res4(3*i+1), res4(3*i+2), 32)})
    val res5          = Wire(Vec(72, UInt(32.W)))
    for(i <- 0 until 36){
        res5(2*i)      := level5(i)(31, 0)
        res5(2*i+1)    := level5(i)(63, 32)
    }

    // stage3: CSA Tree level 6-10
    val res5_reg      = RegInit(VecInit(Seq.fill(72)(0.U(32.W))))
    val en_reg_3      = ShiftRegister(en_reg_2, 1)
    res5_reg          := res5
    // level6: input 72, output 48
    val level6        = VecInit(Seq.tabulate(24){i => CSA(res5_reg(3*i), res5_reg(3*i+1), res5_reg(3*i+2), 32)})
    val res6          = Wire(Vec(48, UInt(32.W)))
    for(i <- 0 until 24){
        res6(2*i)      := level6(i)(31, 0)
        res6(2*i+1)    := level6(i)(63, 32)
    }
    // level7: input 48, output 32
    val level7        = VecInit(Seq.tabulate(16){i => CSA(res6(3*i), res6(3*i+1), res6(3*i+2), 32)})
    val res7          = Wire(Vec(32, UInt(32.W)))
    for(i <- 0 until 16){
        res7(2*i)      := level7(i)(31, 0)
        res7(2*i+1)    := level7(i)(63, 32)
    }
    // level8: input 32, output 22
    val level8        = VecInit(Seq.tabulate(10){i => CSA(res7(3*i), res7(3*i+1), res7(3*i+2), 32)})
    val res8          = Wire(Vec(22, UInt(32.W)))
    for(i <- 0 until 10){
        res8(2*i)      := level8(i)(31, 0)
        res8(2*i+1)    := level8(i)(63, 32)
    }
    res8(20)          := res7(30)
    res8(21)          := res7(31)
    // level9: input 22, output 15
    val level9        = VecInit(Seq.tabulate(7){i => CSA(res8(3*i), res8(3*i+1), res8(3*i+2), 32)})
    val res9          = Wire(Vec(15, UInt(32.W)))
    for(i <- 0 until 7){
        res9(2*i)      := level9(i)(31, 0)
        res9(2*i+1)    := level9(i)(63, 32)
    }
    res9(14)          := res8(21)
    // level10: input 15, output 10
    val level10       = VecInit(Seq.tabulate(5){i => CSA(res9(3*i), res9(3*i+1), res9(3*i+2), 32)})
    val res10         = Wire(Vec(10, UInt(32.W)))
    for(i <- 0 until 5){
        res10(2*i)     := level10(i)(31, 0)
        res10(2*i+1)   := level10(i)(63, 32)
    }

    // stage4: CSA Tree level 11-15
    val res10_reg     = RegInit(VecInit(Seq.fill(10)(0.U(32.W))))
    val en_reg_4      = ShiftRegister(en_reg_3, 1)
    res10_reg         := res10
    // level11: input 10, output 7
    val level11       = VecInit(Seq.tabulate(3){i => CSA(res10_reg(3*i), res10_reg(3*i+1), res10_reg(3*i+2), 32)})
    val res11         = Wire(Vec(7, UInt(32.W)))
    for(i <- 0 until 3){
        res11(2*i)     := level11(i)(31, 0)
        res11(2*i+1)   := level11(i)(63, 32)
    }
    res11(6)          := res10(9)
    // level12: input 7, output 5
    val level12       = VecInit(Seq.tabulate(2){i => CSA(res11(3*i), res11(3*i+1), res11(3*i+2), 32)})
    val res12         = Wire(Vec(5, UInt(32.W)))
    for(i <- 0 until 2){
        res12(2*i)     := level12(i)(31, 0)
        res12(2*i+1)   := level12(i)(63, 32)
    }
    res12(4)          := res11(6)
    // level13: input 5, output 4
    val level13       = VecInit(Seq.tabulate(1){i => CSA(res12(3*i), res12(3*i+1), res12(3*i+2), 32)})
    val res13         = Wire(Vec(4, UInt(32.W)))
    res13(0)          := level13(0)(31, 0)
    res13(1)          := level13(0)(63, 32)
    res13(2)          := res12(3)
    res13(3)          := res12(4)
    // level14: input 4, output 3
    val level14       = VecInit(Seq.tabulate(1){i => CSA(res13(0), res13(1), res13(2), 32)})
    val res14         = Wire(Vec(3, UInt(32.W)))
    res14(0)          := level14(0)(31, 0)
    res14(1)          := level14(0)(63, 32)
    res14(2)          := res13(3)
    // level15: input 3, output 2
    val level15       = VecInit(Seq.tabulate(1){i => CSA(res14(0), res14(1), res14(2), 32)})
    val res15         = Wire(Vec(2, UInt(32.W)))
    res15(0)          := level15(0)(31, 0)
    res15(1)          := level15(0)(63, 32)

    // stage5: adder
    val res15_reg     = RegInit(VecInit(Seq.fill(2)(0.U(32.W))))
    val en_reg_5      = ShiftRegister(en_reg_4, 1)
    res15_reg         := res15
    val res           = res15_reg(0) + res15_reg(1)
    io.res            := res
    

}