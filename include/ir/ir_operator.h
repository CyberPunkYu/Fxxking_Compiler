#ifndef IROP_H
#define IROP_H

#include <string>
// 不用TODO，这个文件不用改
// 对IR操作符进行定义，以枚举类的形式。
// 与 Type 的 toString 函数类似
// 接受一个操作符的枚举类型，返回对应字符串形式。
// 各操作符具体含义可在 IR定义 指导书中查阅。
namespace ir {

enum class Operator {
    _return,    // return   op1
    _goto,      // goto     [op1=cond_var/null],    des = offset
    call,       // call     op1 = func_name,    des = retval  /* func.name = function, func.type = return type*/
    // alloc [arr_size]*4 byte space on stack for array named [arr_name], do not use this for global arrays
    alloc,      // alloc    op1 = arr_size,     des = arr_name
    store,      // store    des,    op1,    op2    op2为下标 -> 偏移量  op1为 store 的数组名, des 为被存储的变量
    load,       // load     des,    op1,    op2    op2为下标 -> 偏移量  op1为 load 的数组名, des 为被赋值变量
    getptr,     // op1: arr_name, op2: arr_off

    def,
    fdef,
    mov,
    fmov,
    cvt_i2f,    // convert [Int]op1 to [Float]des 
    cvt_f2i,    // convert [Float]op1 to [Int]des
    add,
    addi,
    fadd,
    sub,
    subi,
    fsub,
    mul,
    fmul,
    div,
    fdiv,
    mod,
    lss,
    flss,
    leq,
    fleq,
    gtr,
    fgtr,
    geq,
    fgeq,
    eq,
    feq,
    neq,
    fneq,
    _not,
    _and,
    _or
    ,__unuse__
};

std::string toString(Operator t);

}
#endif
