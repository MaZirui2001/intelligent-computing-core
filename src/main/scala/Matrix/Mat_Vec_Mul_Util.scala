import chisel3._
import chisel3.util._

object Mat_Vec_Mul_Func {
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
    def Wallce_Tree_32(src: Vec[UInt]) : Vec[UInt] = {
        // level1: input 32, output 22
        val level1         = VecInit(Seq.tabulate(10){i => CSA(src(3*i), src(3*i+1), src(3*i+2), 32)})
        val res1           = Wire(Vec(22, UInt(32.W)))
        for(i <- 0 until 10){
            res1(2*i)       := level1(i)(31, 0)
            res1(2*i+1)     := level1(i)(63, 32)
        }
        res1(20) := src(30)
        res1(21) := src(31)
        // level2: input 22, output 15
        val level2         = VecInit(Seq.tabulate(7){i => CSA(res1(3*i), res1(3*i+1), res1(3*i+2), 32)})
        val res2           = Wire(Vec(15, UInt(32.W)))
        for(i <- 0 until 7){
            res2(2*i)       := level2(i)(31, 0)
            res2(2*i+1)     := level2(i)(63, 32)
        }
        res2(14) := res1(21)
        // level3: input 15, output 10
        val level3         = VecInit(Seq.tabulate(5){i => CSA(res2(3*i), res2(3*i+1), res2(3*i+2), 32)})
        val res3           = Wire(Vec(10, UInt(32.W)))
        for(i <- 0 until 5){
            res3(2*i)       := level3(i)(31, 0)
            res3(2*i+1)     := level3(i)(63, 32)
        }
        // level4: input 10, output 7
        val level4         = VecInit(Seq.tabulate(3){i => CSA(res3(3*i), res3(3*i+1), res3(3*i+2), 32)})
        val res4           = Wire(Vec(7, UInt(32.W)))
        for(i <- 0 until 3){
            res4(2*i)       := level4(i)(31, 0)
            res4(2*i+1)     := level4(i)(63, 32)
        }
        res4(6) := res3(9)
        // level5: input 7, output 5
        val level5         = VecInit(Seq.tabulate(2){i => CSA(res4(3*i), res4(3*i+1), res4(3*i+2), 32)})
        val res5           = Wire(Vec(5, UInt(32.W)))
        for(i <- 0 until 2){
            res5(2*i)       := level5(i)(31, 0)
            res5(2*i+1)     := level5(i)(63, 32)
        }
        res5(4) := res4(6)
        // level6: input 5, output 4
        val level6         = VecInit(Seq.tabulate(1){i => CSA(res5(3*i), res5(3*i+1), res5(3*i+2), 32)})
        val res6           = Wire(Vec(4, UInt(32.W)))
        for(i <- 0 until 1){
            res6(2*i)       := level6(i)(31, 0)
            res6(2*i+1)     := level6(i)(63, 32)
        }
        res6(2) := res5(3)
        res6(3) := res5(4)
        // level7: input 4, output 3
        val level7         = VecInit(Seq.tabulate(1){i => CSA(res6(3*i), res6(3*i+1), res6(3*i+2), 32)})
        val res7           = Wire(Vec(3, UInt(32.W)))
        for(i <- 0 until 1){
            res7(2*i)       := level7(i)(31, 0)
            res7(2*i+1)     := level7(i)(63, 32)
        }
        res7(2) := res6(3)
        // level8: input 3, output 2
        val level8         = VecInit(Seq.tabulate(1){i => CSA(res7(3*i), res7(3*i+1), res7(3*i+2), 32)})
        val res8           = Wire(Vec(2, UInt(32.W)))
        for(i <- 0 until 1){
            res8(2*i)       := level8(i)(31, 0)
            res8(2*i+1)     := level8(i)(63, 32)
        }
        res8
    }
    def Wallce_Tree_16(src: Vec[UInt]) : Vec[UInt] = {
        // level1: input 16, output 11
        val level1         = VecInit(Seq.tabulate(5){i => CSA(src(3*i), src(3*i+1), src(3*i+2), 32)})
        val res1           = Wire(Vec(11, UInt(32.W)))
        for(i <- 0 until 5){
            res1(2*i)       := level1(i)(31, 0)
            res1(2*i+1)     := level1(i)(63, 32)
        }
        res1(10) := src(15)
        // level2: input 11, output 8
        val level2         = VecInit(Seq.tabulate(3){i => CSA(res1(3*i), res1(3*i+1), res1(3*i+2), 32)})
        val res2           = Wire(Vec(8, UInt(32.W)))
        for(i <- 0 until 3){
            res2(2*i)       := level2(i)(31, 0)
            res2(2*i+1)     := level2(i)(63, 32)
        }
        res2(6) := res1(9)
        res2(7) := res1(10)
        // level3: input 8, output 6
        val level3         = VecInit(Seq.tabulate(2){i => CSA(res2(3*i), res2(3*i+1), res2(3*i+2), 32)})
        val res3           = Wire(Vec(6, UInt(32.W)))
        for(i <- 0 until 2){
            res3(2*i)       := level3(i)(31, 0)
            res3(2*i+1)     := level3(i)(63, 32)
        }
        res3(4) := res2(6)
        res3(5) := res2(7)
        // level4: input 6, output 4
        val level4         = VecInit(Seq.tabulate(2){i => CSA(res3(3*i), res3(3*i+1), res3(3*i+2), 32)})
        val res4           = Wire(Vec(4, UInt(32.W)))
        for(i <- 0 until 2){
            res4(2*i)       := level4(i)(31, 0)
            res4(2*i+1)     := level4(i)(63, 32)
        }
        // level5: input 4, output 3
        val level5         = VecInit(Seq.tabulate(1){i => CSA(res4(3*i), res4(3*i+1), res4(3*i+2), 32)})
        val res5           = Wire(Vec(3, UInt(32.W)))
        for(i <- 0 until 1){
            res5(2*i)       := level5(i)(31, 0)
            res5(2*i+1)     := level5(i)(63, 32)
        }
        res5(2) := res4(3)
        // level6: input 3, output 2
        val level6         = VecInit(Seq.tabulate(1){i => CSA(res5(3*i), res5(3*i+1), res5(3*i+2), 32)})
        val res6           = Wire(Vec(2, UInt(32.W)))
        for(i <- 0 until 1){
            res6(2*i)       := level6(i)(31, 0)
            res6(2*i+1)     := level6(i)(63, 32)
        }
        res6
    }
        

}

