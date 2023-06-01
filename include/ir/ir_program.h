#ifndef PROGRAM_H
#define PROGRAM_H

#include "ir/ir_function.h"
#include "ir/ir_operand.h"

#include <vector>
#include <string>

// 为了实验三翻译为汇编更加方便
// 全局变量必须通过 ir::Program 中的 std::vector<GlobalVal> globalVal 来传递


namespace ir
{
    // 全局变量定义
    // 在后端生成汇编过程中也需对全局变量进行单独处理！
    struct GlobalVal
    {
        ir::Operand val;    //全局变量的值（类型和名字）
        int maxlen = 0;     //为数组长度申请
        GlobalVal(ir::Operand va);
        GlobalVal(ir::Operand va, int len);
    };
    // 程序体定义
    // 实质上是用于添加存放上述函数块
    // 一个输入源程序即对应一个程序体
    // 该源程序中生成的所有IR指令均在程序体中存放
    struct Program {
        std::vector<Function> functions;
        std::vector<GlobalVal> globalVal;
        Program();
        // 用于程序体初始化后向其中条件函数体。
        void addFunction(const Function& proc);
        // 程序体的输出方式定义
        // 除调用各函数体的draw方法外
        // 还定义了全局变量的输出方式
        std::string draw();
    };

}
#endif
