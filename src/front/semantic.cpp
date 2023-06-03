#include"front/semantic.h"

#include<cassert>

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;

#define TODO assert(0 && "TODO");
// get_child_ptr 用于获取根节点的第 index 个孩子节点，并且将其转换为 type 类型的指针，assert 用于确保转换成功
#define GET_CHILD_PTR(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node); 
// analysis 用于对根节点的第 index 个孩子节点进行分析
#define ANALYSIS(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node); analysis##type(node, buffer);
// copy_exp_node 用于将 from 节点的信息复制到 to 节点
#define COPY_EXP_NODE(from, to) to->is_computable = from->is_computable; to->v = from->v; to->t = from->t;
// cvt_nd_opd 用于将 node 节点转换为操作数
#define CVT_ND_OPD(node) Operand(node->v, node->t)
// 静态成员初始化
map<std::string,ir::Function*>* frontend::get_lib_funcs() {
    static map<std::string,ir::Function*> lib_funcs = {
        {"getint", new Function("getint", Type::Int)},
        {"getch", new Function("getch", Type::Int)},
        {"getfloat", new Function("getfloat", Type::Float)},
        {"getarray", new Function("getarray", {Operand("arr", Type::IntPtr)}, Type::Int)},
        {"getfarray", new Function("getfarray", {Operand("arr", Type::FloatPtr)}, Type::Int)},
        {"putint", new Function("putint", {Operand("i", Type::Int)}, Type::null)},
        {"putch", new Function("putch", {Operand("i", Type::Int)}, Type::null)},
        {"putfloat", new Function("putfloat", {Operand("f", Type::Float)}, Type::null)},
        {"putarray", new Function("putarray", {Operand("n", Type::Int), Operand("arr", Type::IntPtr)}, Type::null)},
        {"putfarray", new Function("putfarray", {Operand("n", Type::Int), Operand("arr", Type::FloatPtr)}, Type::null)},
    };
    return &lib_funcs;
}

void frontend::SymbolTable::add_scope(Block* node) {
    TODO;
}
void frontend::SymbolTable::exit_scope() {
    TODO;
}

string frontend::SymbolTable::get_scoped_name(string id) const {
    TODO;
}

Operand frontend::SymbolTable::get_operand(string id) const {
    TODO;
}

frontend::STE frontend::SymbolTable::get_ste(string id) const {
    TODO;
}

frontend::Analyzer::Analyzer(): tmp_cnt(0), symbol_table() {
    TODO;
}

ir::Program frontend::Analyzer::get_ir_program(CompUnit* root) {
    TODO;
}
