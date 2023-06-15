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

namespace frontend
{

// definition of symbol table entry
struct STE {
    ir::Operand operand;    // 记录了符号的名字和类型
    vector<int> dimension;  // 对于数组来说，我们不止需要知道名字和类型，在语义分析的过程中还需要知道维度
    int len = 1;            // 数组的长度，默认为1
    int level = 0;          // 数组维度指针，用于记录当前处理到第几维
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
struct SymbolTable{
    int cnt = 0;
    vector<ScopeInfo> scope_stack;
    map<std::string,ir::Function*> functions;

    /**
     * @brief enter a new scope, record the infomation in scope stacks
     * @param node: a Block node, entering a new Block means a new name scope
     */
    void add_scope(Block*);

    /**
     * @brief exit a scope, pop out infomations
     */
    void exit_scope();

    /**
     * @brief Get the scoped name, to deal the same name in different scopes, we change origin id to a new one with scope infomation,
     * for example, we have these code:
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
     * @param id identifier name
     * @return Operand 
     */
    ir::Operand get_operand(string id) const;

    /**
     * @brief get the right ste with the input name
     * @param id identifier name
     * @return STE 
     */
    STE get_ste(string id) const;
};


// singleton class
struct Analyzer {
    int tmp_cnt;
    vector<ir::Instruction*> g_init_inst;
    SymbolTable symbol_table;
    Analyzer();

    ir::Program program; // 语义分析的结果，程序体
    public:
        ir::Function* current_func; // 指向当前作用的函数体
        Cond* curr_cond;    // 指向当前的条件表达式
        vector<Stmt*> curr_while_stmt;  // 指向当前的 while 循环体
    void insert_ste(std::string name, STE ste);
    

    // analysis functions
    ir::Program get_ir_program(CompUnit*);


    void analyseCompUnit(CompUnit*);
    void analyseDecl(Decl*);
    void analyseConstDecl(ConstDecl*);
    void analyseBType(BType*);
    void analyseConstDef(ConstDef*);
    void analyseConstInitVal(int &, STE&, ConstInitVal*);
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
    void analyseLVal(LVal*);
    void analyseNumber(Number*);
    void analysePrimaryExp(PrimaryExp*);
    void analyseUnaryExp(UnaryExp*);
    void analyseUnaryOp(UnaryOp*);
    std::vector<ir::Operand> analyseFuncRParams(FuncRParams*);
    void analyseMulExp(MulExp*);
    void analyseAddExp(AddExp*);
    void analyseRelExp(RelExp*);
    void analyseEqExp(EqExp*);
    void analyseLAndExp(LAndExp*);
    void analyseLOrExp(LOrExp*);
    void analyseConstExp(ConstExp*);

    // reject copy & assignment
    Analyzer(const Analyzer&) = delete;
    Analyzer& operator=(const Analyzer&) = delete;
};

} // namespace frontend

#endif