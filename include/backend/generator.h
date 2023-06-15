#ifndef GENERARATOR_H
#define GENERARATOR_H

#include "ir/ir.h"
#include "backend/rv_def.h"
#include "backend/rv_inst_impl.h"
#include "front/semantic.h"

#include<set>
#include<map>
#include<string>
#include<vector>
#include<fstream>
using namespace rv;

namespace backend {

struct operandCmp {
    bool operator() (const ir::Operand& a, const ir::Operand& b) const;
};

// 将变量与栈指针偏移量做映射
// the mem addr of a local variable can be identified by ($s0 + off)
/**
 * @brief   变量与偏移量的映射 只是一个逻辑映射
 *          产生汇编代码时认为变量的值存放与此
 *          你需要产生汇编代码来完成内存的分配
 *          使用 load/store 操作相应地址来 读取/存放 变量的值
 */
struct stackVarMap {
    int offset; // offset to s0
    const std::vector<ir::GlobalVal> &globalVal;
    stackVarMap(const std::vector<ir::GlobalVal> &gb);

    std::map<ir::Operand, int, operandCmp> _table;
    // 局部变量寻址，返回的偏移量 + 栈指针来实现
    int find_operand(ir::Operand);
    // 添加变量并维护变量与偏移量的映射
    int add_operand(ir::Operand, uint32_t size = 4);
};


class regAllocator {

public:
    std::map<ir::Operand, rvREG, operandCmp> op2reg_map;
    std::vector<ir::Operand> reg2op_map;    
    std::set<rvREG> available_regs;
    
    std::set<std::pair<int, rvREG>> reg_using;
    std::vector<int> reg_timestamp;
    std::vector<rv::rv_inst*> &rv_insts;
    stackVarMap stack_var_map;
    regAllocator(std::vector<rv::rv_inst*> &rv_insts, const std::vector<ir::GlobalVal> &globalVal);

    void update(rvREG r, int time);
    void spill(rvREG r);
    void load(rvREG r, ir::Operand op, int time, int needload);
    rv::rvREG getReg(ir::Operand, int time, int needload);
    void clearregs();
};


struct Generator {
    const ir::Program& program;         // the program to gen
    std::ofstream& fout;                 // output file

    Generator(ir::Program&, std::ofstream&);

    regAllocator *reg_allocator;
    std::vector<rv_inst*> *rv_insts;
    std::vector<int> *goto_label_lines;
    std::set<rv_inst*> *ret_set;
    int max_call_overflow_paras;
    int global_label_id;

    // generate wrapper function
    int get_label_id(int line_num);
    void gen();
    void gen_func(const ir::Function&);
    void gen_instr(const ir::Instruction&, int time);
};

} // namespace backend


#endif