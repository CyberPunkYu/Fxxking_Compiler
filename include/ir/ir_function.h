#ifndef IRFUNCTION_H
#define IRFUNCTION_H
#include <vector>
#include <string>
#include "ir/ir_operand.h"
#include "ir/ir_instruction.h"
namespace ir
{

// 不用TODO了，这里已经实现了

// 函数块定义
// 用于添加存放输入源程序中某个函数生成的IR指令
struct Function {
    // 函数块名称，可以直接将源程序中函数名作为name
    std::string name;
    // 函数返回值类型
    // 对于全局以及void类型，ir::Type中的null就派上用场了
    ir::Type returnType;
    // 函数形参列表。该列表可以为空
    std::vector<Operand> ParameterList;
    // 函数对应的IR指令
    std::vector<Instruction*> InstVec;
    Function();
    // 无形参情况
    Function(const std::string&, const ir::Type&);
    // 有形参情况
    Function(const std::string&, const std::vector<Operand>&, const ir::Type&);
    // 添加指令
    void addInst(Instruction* inst);
    // 重写draw方法，用于输出IR指令
    // 除调用 InstVec 中各指令的 draw 方法外
    // 还需要输出函数名、返回值类型、形参列表
    std::string draw();
};

}
#endif
