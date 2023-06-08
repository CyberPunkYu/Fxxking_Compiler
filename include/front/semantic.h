/**
 * @file semantic.h
 * @author Yuntao Dai (d1581209858@live.com)
 * @brief 
 * @version 0.1
 * @date 2023-01-06
 * 
 * a Analyzer should 
 * @copyright Copyright (c) 2023
 * 
 */

// 符号表

#ifndef SEMANTIC_H
#define SEMANTIC_H

#include"ir/ir.h"
#include"front/abstract_syntax_tree.h"

#include<map>
#include<string>
#include<vector>

using std::map;
using std::string;
using std::vector;

namespace frontend{

// definition of symbol table entry
struct STE {
    ir::Operand operand;    // 记录了符号的名字和类型
    vector<int> dimension;  // 对于数组来说，我们不止需要知道名字和类型，在语义分析的过程中还需要知道维度
};

// 存储符号表的数据结构，key是符号的名字，value是STE
using map_str_ste = map<string, STE>;
// definition of scope infomation
// 为支持重命名, 我们需要一个数据结构来存储作用域相关的信息
// 每个作用域有自己的属性来唯一标识自己
// 还有一张表来存储这个作用域里所有变量的名称和类型
struct ScopeInfo {
    int cnt;    // 函数中的唯一编号, 代表是函数中出现的第几个作用域
    string name;// 用来分辨作用域的类别, 'b' 代表是一个单独嵌套的作用域, 'i' 'e' 'w' 分别代表由 if else while 产生的新作用域
    map_str_ste table;  // 一张存放符号的表, {string: STE}, string 是操作数的原始名称，表项 STE 实际上就是一个 IR 的操作数，即 STE -> Operand, 在 STE 中存放的应该是变量重命名后的名称
};

// surpport lib functions
map<std::string,ir::Function*>* get_lib_funcs();

// definition of symbol table
// 符号表的数据结构，由于需要支持嵌套作用域，所以我们需要一个栈来存储作用域的信息
struct SymbolTable{
    // 用于标识当前作用域的编号
    int cnt = 0;
    vector<ScopeInfo> scope_stack;
    // 用来存储函数的信息，key 是函数的名字，value 是函数的 IR 表示
    map<std::string,ir::Function*> functions;

    /**
     * @brief enter a new scope, record the infomation in scope stacks
     * @brief 进入新作用域时, 向符号表中添加 ScopeInfo, 相当于压栈
     * @param node: a Block node, entering a new Block means a new name scope
     */
    void add_scope(Block*);

    /**
     * @brief exit a scope, pop out infomations
     * @brief 退出时弹栈
     */
    void exit_scope();

    /**
     * @brief Get the scoped name, to deal the same name in different scopes, we change origin id to a new one with scope infomation,
     * for example, we have these code:
     * @brief 输入一个变量名, 返回其在当前作用域下重命名后的名字 (相当于加后缀)
     * "     
     * int a;
     * {
     *      int a; ....
     * }
     * "
     * in this case, we have two variable both name 'a', after change they will be 'a' and 'a_block'
     * @param id: origin id 
     * @return string: new name with scope infomations
     */
    string get_scoped_name(string id) const;

    /**
     * @brief get the right operand with the input name
     * @brief 输入一个变量名, 在符号表中寻找最近的同名变量, 返回对应的 Operand(注意，此 Operand 的 name 是重命名后的)
     * @param id identifier name
     * @return Operand 
     */
    ir::Operand get_operand(string id) const;

    /**
     * @brief get the right ste with the input name
     * @brief 输入一个变量名, 在符号表中寻找最近的同名变量, 返回 STE
     * @param id identifier name
     * @return STE 
     */
    STE get_ste(string id) const;
};


// singleton class
// 语义分析器，本次实验目标
struct Analyzer {
    int tmp_cnt;    // 临时变量的计数器
    vector<ir::Instruction*> g_init_inst;   // 全局变量初始化的指令
    SymbolTable symbol_table;   // 符号表
    Analyzer(); 

    ir::Program program; // 语义分析的结果，程序体
    ir::Function* curr_function; // 指向当前作用的函数体

    Cond* last_cond; // 最终的条件
    vector<Stmt*> last_while;

    // 向符号表中添加变量
    void insert_ste(std::string name, STE ste);

    // analysis functions
    // 语义分析的入口
    /**
     * @brief Get the ir program object
     * @param CompUnit* : AST 的根节点
     * @return ir::Program 
     */
    ir::Program get_ir_program(CompUnit*);

    // reject copy & assignment
    Analyzer(const Analyzer&) = delete;
    Analyzer& operator=(const Analyzer&) = delete;

    // 各节点的语义分析函数
    void analyseCompUnit(CompUnit*);
    void analyseDecl(Decl*);
    void analyseConstDecl(ConstDecl*);
    void analyseBType(BType*);
    void analyseConstDef(ConstDef*);
    void analyseConstInitVal(int &, STE&, int, int, ConstInitVal*);
    void analyseVarDecl(VarDecl*);
    void analyseVarDef(VarDef*);
    void analyseInitVal(int&, STE&, int, int, InitVal*);
    void analyseFuncDef(FuncDef*);
    void analyseFuncType(FuncType*);
    void analyseFuncFParam(FuncFParam*);
    void analyseFuncFParams(FuncFParams*);
    void analyseBlock(Block*);
    void analyseBlockItem(BlockItem*);
    void analyseStmt(Stmt*);
    void analyseExp(Exp*);
    void analyseCond(Cond*);
    void analyseLVal(LVal*, int);
    void analyseNumber(Number*);
    void analysePrimaryExp(PrimaryExp*);
    void analyseUnaryExp(UnaryExp*);
    void analyseUnaryOp(UnaryOp*);
    void analyseFuncRParams(std::vector<ir::Operand>&, FuncRParams*);
    void analyseMulExp(MulExp*);
    void analyseAddExp(AddExp*);
    void analyseRelExp(RelExp*);
    void analyseEqExp(EqExp*);
    void analyseLAndExp(LAndExp*);
    void analyseLOrExp(LOrExp*);
    void analyseConstExp(ConstExp*);
};

} // namespace frontend

#endif