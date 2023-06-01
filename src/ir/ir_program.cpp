#include "ir/ir_program.h"

#include <vector>
#include <string>
#include <iostream>

// 为了实验三翻译为汇编更加方便
// 全局变量必须通过 ir::Program 中的 std::vector<GlobalVal> globalVal 来传递

ir::Program::Program(): functions(std::vector<ir::Function>()) {}

ir::GlobalVal::GlobalVal(ir::Operand va) {this->val = va;}

ir::GlobalVal::GlobalVal(ir::Operand va, int len) {this->val = va; this->maxlen = len;}

void ir::Program::addFunction(const ir::Function& proc) {
    functions.push_back(proc);
}

std::string ir::Program::draw() {
    std::string ret;
    for (auto i :functions) {
        ret += i.draw();
    }
    ret += "GVT:\n";
    for (auto i : this->globalVal) {
        ret += "\t" + i.val.name + " " + toString(i.val.type) + " " + std::to_string(i.maxlen) + "\n";
    }
    return ret;
}