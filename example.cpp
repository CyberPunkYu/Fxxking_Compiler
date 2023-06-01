//IR测试样例
#include <iostream>
#include "ir/ir.h"
#include "tools/ir_executor.h"

/*
int a;
int arr[2] = { 2, 4};
int func(int p){
    p = p - 1;
    return p;
}
int main(){
    int b;
    a = arr[1];
    b = func(a);
    if (b < a) b = b * 2;
    return b;
}
*/
/*
void global()
    0: def a, 0
    1: alloc arr, 2
    2: store 2, arr, 0
    3: store 4, arr, 1
    4: return null
end

int func(int p)
    0: subi t1, p, 1
    1: mov p, t1
    2: return p
end

int main()
    0: call t0, global()
    1: def b, 0
    2: load t2, arr, 1
    3: mov a, t2
    4: call t2, func(a)
    5: mov b, t2
    6: lss t3, b, a
    7: if t3 goto [pc, 2]
    8: goto [pc, 4]
    9: def t4, 2
    10: mul t5, b, t4
    11: mov b, t5
    12: return b
end

GVT:
    a int 0
    arr int* 2
*/


// program 程序体
// function 函数
// instruction 指令
// operand 操作数
// operator 操作符


int main() {
    // 每个测试样例都需要重新初始化，初始化即为new一个Program程序体
    ir::Program program;
    ir::Function globalFunc("global", ir::Type::null);  // 全局函数, 无返回值
    // Instruction是为了构造IR指令的类，每个指令都需要构造一个Instruction类
    // def a 0
    ir::Instruction assignInst(ir::Operand("0",ir::Type::IntLiteral),   // 赋值指令，将0赋值给a，类型为int
                                ir::Operand(),
                                ir::Operand("a",ir::Type::Int),
                                ir::Operator::def);
    // alloc arr 2
    ir::Instruction allocInst(ir::Operand("2",ir::Type::IntLiteral),
                              ir::Operand(),
                              ir::Operand("arr",ir::Type::IntPtr),
                              ir::Operator::alloc);
    // store 2 arr 0
    ir::Instruction storeInst(ir::Operand("arr",ir::Type::IntPtr),
                              ir::Operand("0",ir::Type::IntLiteral),
                              ir::Operand("2",ir::Type::IntLiteral),
                              ir::Operator::store);
    // store 4 arr 1
    ir::Instruction storeInst1(ir::Operand("arr",ir::Type::IntPtr),
                              ir::Operand("1",ir::Type::IntLiteral),
                              ir::Operand("4",ir::Type::IntLiteral),
                              ir::Operator::store);
    // return null
    ir::Instruction globalreturn(ir::Operand(),
                                 ir::Operand(),
                                 ir::Operand(),
                                 ir::Operator::_return);
    // 将global指令加入到函数中
    globalFunc.addInst(&allocInst);
    globalFunc.addInst(&assignInst);
    globalFunc.addInst(&storeInst);
    globalFunc.addInst(&storeInst1);
    globalFunc.addInst(&globalreturn);
    // 添加程序体中的全局变量，这里是a和arr
    program.globalVal.emplace_back(ir::Operand("a", ir::Type::Int));
    program.globalVal.emplace_back(ir::Operand("arr", ir::Type::IntPtr), 2); // 由于arr是数组，所以需要指定数组大小，且类型为IntPtr
    // 添加函数到程序体中
    program.addFunction(globalFunc);

    // 记录func函数的参数
    std::vector<ir::Operand> paraVec = {ir::Operand("p",ir::Type::Int)}; //int p
    // 构造Function类，参数分别为函数名，参数列表，返回值类型
    ir::Function funcFunction("func", paraVec, ir::Type::Int);
    // subi t1 p 1
    ir::Instruction subInst(ir::Operand("p",ir::Type::Int),
                            ir::Operand("1",ir::Type::IntLiteral),
                            ir::Operand("t1",ir::Type::Int),    // 其中计算结果存入t1中
                            ir::Operator::subi);
    // mov p t1
    ir::Instruction movInst(ir::Operand("t1",ir::Type::Int),
                                        ir::Operand(),
                                        ir::Operand("p", ir::Type::Int),
                                        ir::Operator::mov);
    // return p
    ir::Instruction returnInst(ir::Operand("p",ir::Type::Int),
                               ir::Operand(),
                               ir::Operand(),
                               ir::Operator::_return);
    // 将指令加入到函数中
    funcFunction.addInst(&subInst);
    funcFunction.addInst(&movInst);
    funcFunction.addInst(&returnInst);
    // 将函数加入到程序体中
    program.addFunction(funcFunction);

/*
int main(){
    int b;
    a = arr[1];
    b = func(a);
    if (b < a) b = b * 2;
    return b;
}
*/
    // 构造main函数
    ir::Function mainFunction("main",ir::Type::Int);
    // main函数首先会调用global函数
    ir::CallInst callGlobal(ir::Operand("global",ir::Type::null),
                            ir::Operand("t0",ir::Type::null));
    // def b 0
    ir::Instruction defInst(ir::Operand("0",ir::Type::IntLiteral),
                            ir::Operand(),
                            ir::Operand("b",ir::Type::Int),
                            ir::Operator::def);
    // load t2 arr 1
    ir::Instruction loadInst(ir::Operand("arr",ir::Type::IntPtr),
                             ir::Operand("1",ir::Type::IntLiteral),
                             ir::Operand("t2",ir::Type::Int),
                             ir::Operator::load);
    // mov a t2
    ir::Instruction movInst1(ir::Operand("t2",ir::Type::Int),
                             ir::Operand(),
                             ir::Operand("a",ir::Type::Int),
                             ir::Operator::mov);
    // 记录func函数的参数  b = func(a); 需要将a作为参数传入func函数
    std::vector<ir::Operand> paraVec1 = {ir::Operand("a",ir::Type::Int)};
    // call t2 func(a) 之后会调用func函数，将返回值存入t2中
    ir::CallInst callInst(ir::Operand("func",ir::Type::Int),
                          paraVec1,
                          ir::Operand("t2",ir::Type::Int));
    // mov b t2
    ir::Instruction movInst2(ir::Operand("t2",ir::Type::Int),
                             ir::Operand(),
                             ir::Operand("b",ir::Type::Int),
                             ir::Operator::mov);
    // lss b a 如果b < a 则t3 = 1
    ir::Instruction lssInst(ir::Operand("b",ir::Type::Int),
                            ir::Operand("a",ir::Type::Int),
                            ir::Operand("t3",ir::Type::Int),
                            ir::Operator::lss);
    // if t3 goto 2 如果t3 == 1 则跳转到后面第二条指令
    ir::Instruction gotoInst(ir::Operand("t3",ir::Type::Int),
                             ir::Operand(),
                             ir::Operand("2",ir::Type::IntLiteral),
                             ir::Operator::_goto);
    // 否则跳转到后面第四条指令
    ir::Instruction gotoInst1(ir::Operand(),
                              ir::Operand(),
                              ir::Operand("4",ir::Type::IntLiteral),
                              ir::Operator::_goto);
    // def t4 2 对于立即数的乘除运算，需要先将立即数存入一个临时变量中，这里是等式成立后跳转到的指令
    ir::Instruction defInst2(ir::Operand("2",ir::Type::IntLiteral),
                             ir::Operand(),
                             ir::Operand("t4",ir::Type::Int),
                             ir::Operator::def);
    // mul t5 b t4 将b * 2的结果存入t5中
    ir::Instruction mulInst(ir::Operand("b",ir::Type::Int),
                            ir::Operand("t4",ir::Type::Int),
                            ir::Operand("t5",ir::Type::Int),
                            ir::Operator::mul);
    // mov b t5 将t5的值存入b中
    ir::Instruction movInst3(ir::Operand("t5",ir::Type::Int),
                             ir::Operand(),
                             ir::Operand("b",ir::Type::Int),
                             ir::Operator::mov);
    // return b 这里是等式不成立后跳转到的指令，即直接返回b的值
    ir::Instruction returnInst1(ir::Operand("b",ir::Type::Int),
                                ir::Operand(),
                                ir::Operand(),
                                ir::Operator::_return);
    mainFunction.addInst(&callGlobal);
    mainFunction.addInst(&defInst);
    mainFunction.addInst(&loadInst);
    mainFunction.addInst(&movInst1);
    mainFunction.addInst(&callInst);
    mainFunction.addInst(&movInst2);
    mainFunction.addInst(&lssInst);
    mainFunction.addInst(&gotoInst);
    mainFunction.addInst(&gotoInst1);
    mainFunction.addInst(&defInst2);
    mainFunction.addInst(&mulInst);
    mainFunction.addInst(&movInst3);
    mainFunction.addInst(&returnInst1);
    program.addFunction(mainFunction);
    std::cout << program.draw();
    // 进行验证
    ir::Executor executor(&program);
    std::cout << executor.run();
}