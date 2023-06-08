#include"front/semantic.h"
#include<exception>
#include<cassert>

/* 本实验将放弃Float类型的支持 */

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;

#define TODO assert(0 && "TODO");

// copy_exp_node 用于将 from 节点的信息复制到 to 节点
#define COPY_EXP_NODE(from, to) to->is_computable = from->is_computable; \
                                to->v = from->v; \
                                to->t = from->t;
// 生成临时变量名称
#define GET_TMP_NAME ("t" + std::to_string(tmp_cnt++))
// 赋值语句，将整型字面量赋值给一个整型的临时变量
#define ASSIGN_INT_TO_TMP(op) Operand des(GET_TMP_NAME, Type::Int); \
                              INST_INSERT(op, Operand(), des, Operator::mov); \
                              return des;
// 指令插入
#define INST_INSERT(op1, op2, des, op) Instruction* inst = new Instruction(op1, op2, des, op); \
                                       curr_function->addInst(inst);
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
    // 该Scope的编号
    cnt ++;
    int scopeID = cnt;
    // 该Scope的name，感觉对Scope分类并没有什么用，所以就用编号来表示了
    string scope_name = std::to_string(scopeID);
    // 初始化符号表
    ScopeInfo scope_info;
    scope_info.name = scope_name;
    // 将该Scope的信息加入符号表
    scope_stack.push_back(scope_info);
}

// 退出Scope
void frontend::SymbolTable::exit_scope() {
    scope_stack.pop_back();
}

string frontend::SymbolTable::get_scoped_name(string id) const {
    // 从栈顶往下遍历，如果找到该id的变量，就返回该变量的id+scope_name
    for(int i = scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.count(id)){
            return id + "_" + scope_stack[i].name;
        }
    }
    // 如果都没找到，则是首次声明，直接返回原来的id
    return id;
}

Operand frontend::SymbolTable::get_operand(string id) const {
    // 从栈顶往下遍历
    for(int i = scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            // 返回操作数
            return scope_stack[i].table.at(id).operand;
        }
    }
    return Operand();
}

// 实现与get_operand类似的功能，但是不返回操作数，而是返回STE
frontend::STE frontend::SymbolTable::get_ste(string id) const {
    // 从栈顶往下遍历
    for(int i = scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            return scope_stack[i].table.at(id);  //注：这里at直接返回value，find是返回迭代器
        }
    }
    return STE();
}

// Analysis构造函数
frontend::Analyzer::Analyzer(): tmp_cnt(0), symbol_table(),curr_function(nullptr){
    // 初始化程序体符号表
    symbol_table.cnt = 0;
    symbol_table.scope_stack = vector<frontend::ScopeInfo>();
    // 添加静态函数
    symbol_table.functions = *get_lib_funcs();
    // 添加全局作用域
    ScopeInfo scope_info;
    scope_info.name = "global";
    scope_info.cnt  = 0;
    symbol_table.scope_stack.push_back(scope_info);
}

// 本次实验的接口
ir::Program frontend::Analyzer::get_ir_program(CompUnit* root) {
    analyseCompUnit(root);
    // 添加全局变量初始化指令
    Function* globalFunc = new Function("global", ir::Type::null);
    for(auto i : g_init_inst) globalFunc->addInst(i);
    globalFunc->addInst(new Instruction(Operand(), Operand(), Operand(), Operator::_return));
    // 添加全局函数到程序体中
    program.addFunction(*globalFunc);
    program.draw();
    return program;
}

// CompUnit -> (Decl | FuncDef) [CompUnit]
// Decl -> ConstDecl | VarDecl
// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
// 1. int main() {int a;}
void frontend::Analyzer::analyseCompUnit(CompUnit* root){
    
}