#ifndef IRINSTRUCTION_H
#define IRINSTRUCTION_H

#include "ir/ir_operand.h"
#include "ir/ir_operator.h"

#include <vector>
#include <string>

// 不用TODO了，这里已经实现了
namespace ir
{
    // IR 指令的基类定义
    // 两个源操作数与结果操作数以及Operator类型
    struct Instruction {
        Operand op1;
        Operand op2;
        Operand des;
        Operator op;
        Instruction();
        Instruction(const Operand& op1, const Operand& op2, const Operand& des, const Operator& op);
        // 定义了各类型指令输出格式，并以字符串形式返回
        virtual std::string draw() const;
    };
    // 由于函数调用指令较为特殊，需额外传入函数调用实参，这里对其进行额外定义。
    struct CallInst: public Instruction{
        // 用于存入函数调用实参。
        // 该类中也对Instruction基类中draw方法进行重写。
        std::vector<Operand> argumentList;
        CallInst(const Operand& op1, std::vector<Operand> paraList, const Operand& des);
        CallInst(const Operand& op1, const Operand& des);   //无参数情况
        std::string draw() const;
    };
}
#endif
