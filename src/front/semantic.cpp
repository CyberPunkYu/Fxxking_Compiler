#include"front/semantic.h"
#include<exception>
#include<cassert>

/* 本实验将放弃Float类型的支持 */

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;

#define TODO assert(0 && "TODO");

// 返回根节点的某个孩子节点
#define GET_CHILD(type, index) dynamic_cast<type *>(root->children[index])
// 将根节点的孩子递归分析
#define ANALYSIS(node, type, index) auto node = dynamic_cast<type *>(root->children[index]); \
                                    assert(node);                                            \
                                    analyse##type(node);
#define ANALYSIS_WITH_NODE(node, type) analyse##type((type*)node);
// copy_exp_node 用于将 from 节点的信息复制到 to 节点
#define COPY_EXP_NODE(from, to) to->is_computable = from->is_computable; \
                                to->v = from->v; \
                                to->t = from->t;
// 生成临时变量名称
#define GET_TMP_NAME ("t" + std::to_string(tmp_cnt++))
// 赋值语句，将整型字面量赋值给一个整型的临时变量
#define ASSIGN_INT_TO_TMP(op, opname, instname) Operand opname(GET_TMP_NAME, Type::Int); \
                                                Instruction* instname = new Instruction(op, Operand(), des, Operator::mov); \
                                                curr_function->addInst(instname);
// 指令插入
#define INST_INSERT(op1, op2, des, op, name) auto name = new Instruction(op1, op2, des, op); \
                                             curr_function->addInst(name);
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
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            return id + "_" + scope_stack[i].name;
        }
    }
    return id;
}

Operand frontend::SymbolTable::get_operand(string id) const {
    // 从栈顶往下遍历
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
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
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
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
    scope_info.cnt  = symbol_table.cnt;
    symbol_table.scope_stack.push_back(scope_info);
}

// 向符号表中插入STE
void frontend::Analyzer::insert_ste(std::string name, STE ste) {
    symbol_table.scope_stack.back().table[name] = ste;
    // 如果是全局变量，还要加入到program中
    if(curr_function == nullptr) {
        ste.operand.name = ste.operand.name + "_global";
        if(ste.operand.type == Type::IntPtr) {
            int len = 1;
            // 初始化全局变量的长度，注意并没有二维变量的概念
            for(int i : ste.dimension) { len *= i; }
            program.globalVal.push_back(ir::GlobalVal(ste.operand, len));
        } 
        else{ program.globalVal.push_back(ir::GlobalVal(ste.operand)); } 
    }
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

// CompUnit -> (Decl | FuncDef) [CompUnit] **
void frontend::Analyzer::analyseCompUnit(CompUnit* root){
    // 如果是Decl
    if(root->children[0]->type == NodeType::DECL){ ANALYSIS(decl, Decl, 0); }
    // 否则为FuncDef
    else{ ANALYSIS(funcdef, FuncDef, 0); }
    // 如果还有CompUnit，递归分析
    if(root->children.size() > 1){ ANALYSIS(compunit, CompUnit, 1); }
}

// Decl -> ConstDecl | VarDecl **
void frontend::Analyzer::analyseDecl(Decl* root){
    // 如果是ConstDecl
    if(root->children[0]->type == NodeType::CONSTDECL){ ANALYSIS(constdecl, ConstDecl, 0); }
    // 否则为VarDecl
    else{ ANALYSIS(vardecl, VarDecl, 0); }
}

// ConstDecl -> 'const' BType ConstDef {',' ConstDef}';'
// Type t; **
void frontend::Analyzer::analyseConstDecl(ConstDecl* root){
    // 分析BType
    ANALYSIS(btype, BType, 1);
    // root节点的类型为BType的类型
    root->t = btype->t;
    // 分析ConstDef
    for(int i = 2; i < (int) root->children.size() - 1; i += 2){
        // 将该变量的类型设置为声明的类型
        ConstDef* constdef = GET_CHILD(ConstDef, i);
        constdef->t = root->t;
        ANALYSIS_WITH_NODE(constdef, ConstDef);
    }
}

// BType -> 'int' | 'float'
// BType.t **
void frontend::Analyzer::analyseBType(BType* root){
    Term* term = GET_CHILD(Term, 0);
    // 我们仅考虑int
    if(term->token.type == TokenType::INTTK){ root->t = Type::Int; }

}

// ConstDef -> Ident { '[' ConstExp ']' } '=' ConstInitVal
// ConstInitVal -> ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
// std::string arr_name;
// Type t;
// 这里出现了常量的定义，需要将常量的值存入符号表中 debug
void frontend::Analyzer::analyseConstDef(ConstDef* root){
    Term* term = GET_CHILD(Term, 0);
    // 一旦出现了中括号，就是数组
    bool is_array = (GET_CHILD(Term, 1))->token.type == TokenType::LBRACK;
    // ConstInitVal 出现的位置
    int default_pos = 2;
    int index = 0;
    // 对于数组来说的维度
    vector<int> dimension;
    // 初始化STE
    STE ste;
    // 如果是数组
    if(is_array){
        // 首先先确定ConstInitVal出现的位置
        while(root->children[default_pos]->type == NodeType::CONSTEXP){
            // 分析ConstExp
            ANALYSIS(constexp, ConstExp, default_pos);
            // 如果是ConstExp，那么多出现一维
            int dim = std::stoi(constexp->v);
            dimension.push_back(dim);
            ste.len *= dim;
            // 如果多一维，那么ConstInitVal出现的位置需要向后移动3位
            // a = 2(2) --> a[2] = 2(5)
            default_pos += 3;
        }
        // 初始化STE
        ste.dimension = dimension;
        ste.operand = Operand(term->token.value, Type::IntPtr);
        insert_ste(ste.operand.name, ste);
        // 分配内存空间，op1即为数组长度，des即为声明的变量
        Operand op1 = Operand(std::to_string(ste.len), Type::IntLiteral);
        Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), Type::IntPtr);
        // 生成指令
        INST_INSERT(op1, {}, des, Operator::alloc, alloc);
        // 分析ConstInitVal
        analyseConstInitVal(index, ste, GET_CHILD(ConstInitVal, default_pos));
    }
    else{
        ste.operand = Operand(term->token.value, root->t == Type::Int? Type::IntLiteral : Type::FloatLiteral);
        // 先分析ConstInitVal
        analyseConstInitVal(index, ste, GET_CHILD(ConstInitVal, default_pos));
        // 将STE插入符号表，此处需要修改
        // insert_ste(ste.operand.name, ste);
        insert_ste(term->token.value, ste);
    }
}

// ConstInitVal -> ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
// ConstInitVal.v
// ConstInitVal.t
// 由于我们可能会分析数组，所以需要传入当前维度的下表index，以及当前分析的维度level debug
void frontend::Analyzer::analyseConstInitVal(int &index, STE& ste, ConstInitVal* root) {
    // 判断是否是数组
    bool is_array = root->children[0]->type == NodeType::CONSTEXP;
    // ConstExp
    if(is_array){
        // 分析ConstExp
        ANALYSIS(constexp, ConstExp, 0);
        // 如果是int立即数
        if(ste.operand.type == Type::IntLiteral){ ste.operand.name = constexp->v; }
        // 如果是数组
        else{
            // 数组名
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), Type::IntPtr);
            // 数组下标
            Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
            // 数组值
            Operand op3 = Operand(constexp->v, Type::IntLiteral);
            // 生成指令
            ASSIGN_INT_TO_TMP(op3, des, mov);
            INST_INSERT(op1, op2, des, Operator::store, store);
        }
        // 位置向后移动
        index++;
    }
    // '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    else{
        // 需要分析的ConstInitVal的位置
        int default_pos = 1;
        // 分析所有的ConstInitVal
        while(default_pos < (int) root->children.size() - 1){
            // 每分析一维，剩余待分析的数量减少
            ste.len /= ste.dimension[ste.level];
            ste.level++;
            // 分析ConstInitVal
            analyseConstInitVal(index, ste, (ConstInitVal*) root->children[default_pos]);
            // 位置向后移动
            default_pos += 2;
        }
    }
    // 使用完len和level后，需要将其还原！！
    ste.level = 0;
    for(auto i : ste.dimension){ste.len *= i;}
}

// VarDecl -> BType VarDef { ',' VarDef } ';'
// VarDecl.t **
void frontend::Analyzer::analyseVarDecl(VarDecl* root){
    // 不需要分析BType，因为我们仅考虑int
    root->t = Type::Int;
    for(int i = 1; i < (int) root->children.size() - 1; i += 2){ ANALYSIS(vardef, VarDef, i); }
}

// VarDef -> Ident { '[' ConstExp ']' } [ '=' InitVal ]
// VarDef.arr_name
// 操作和ConstDef类似 **
void frontend::Analyzer::analyseVarDef(VarDef* root){
    Term* term = GET_CHILD(Term, 0);
    bool is_array = root->children.size() > 1 && GET_CHILD(Term, 1)->token.type == TokenType::LBRACK;
    int default_pos = 2;
    STE ste;
    std::vector<int> dimension;
    // 如果是数组
    if(is_array) {
        while(default_pos < (int) root->children.size() && root->children[default_pos]->type == NodeType::CONSTEXP) {
            ANALYSIS(constExp, ConstExp, default_pos);
            int dim = std::stoi(constExp->v);
            dimension.push_back(dim);
            default_pos += 3;
            ste.len *= dim;
        }
        // 将数组的维度存入符号表
        ste.operand = Operand(term->token.value, Type::IntPtr);
        ste.dimension = dimension;
        insert_ste(ste.operand.name, ste);
        if(curr_function != nullptr) {
            // 只有局部变量才需要alloc，全局变量不需要alloc
            Operand op1 = Operand(std::to_string(ste.len), Type::IntLiteral);
            Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            INST_INSERT(op1, {}, des, Operator::alloc, alloc);
        }
    } 
    // 如果不是数组
    else {
        // 将变量的类型设置为int
        ste.operand = Operand(term->token.value, Type::Int);
        insert_ste(ste.operand.name, ste);
    }
    // 如果有初始化
    if(default_pos < (int) root->children.size()) {
        int index = 0;
        analyseInitVal(index, ste, ste.len, ste.level, (InitVal*) root->children[default_pos]);
    }
    else if(curr_function == nullptr) {
        // 如果是全局变量，需要初始化成全0
        if(is_array) {
            // 全局数组不需要初始化
        } 
        else {
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand des = Operand("0", Type::IntLiteral);
            INST_INSERT(op1, {}, des, Operator::def, def);
        }
    }
    else{}
}

// InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}'
// InitVal.is_computable
// InitVal.v
// InitVal.t
// 和ConstInitVal类似 **
void frontend::Analyzer::analyseInitVal(int& index, STE& ste, int res, int level, InitVal* root){
    bool is_array = root->children[0]->type == NodeType::EXP;
    if(!is_array) {
        int default_pos = 1;
        int init_index = index;
        while(default_pos < (int) root->children.size() && root->children[default_pos]->type == NodeType::INITVAL) {
            analyseInitVal(index, ste, res / ste.dimension[level], level + 1, (InitVal*) root->children[default_pos]);
            default_pos += 2;
        }
        // for(; index < init_index + res; index++) {
        //     Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
        //     Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
        //     Operand op3 = Operand("0", Type::IntLiteral);
        //     ASSIGN_INT_TO_TMP(op3, des, mov);
        //     INST_INSERT(op1, op2, des, Operator::store, store);
        // }
    } else {
        // calc constexp val
        ANALYSIS(exp, Exp, 0);
        // store
        // 如果是数组
        if(ste.operand.type == Type::IntPtr) {
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
            // 如果是常数，需要将常数赋值给临时变量
            if(exp->is_computable) {
                Operand op3 = Operand(exp->v, Type::IntLiteral);
                ASSIGN_INT_TO_TMP(op3, des, mov);
                INST_INSERT(op1, op2, des, Operator::store, store);
            }
            // 如果是变量，直接存储
            else {
                Operand des = Operand(exp->v, Type::Int);
                INST_INSERT(op1, op2, des, Operator::store, store);
            }
            index++;
        }
        // 不是数组
        else {
            // 初始化
            Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand op1;
            op1.name = exp->v;
            if(exp->is_computable) { op1.type = Type::IntLiteral; }
            else { op1.type = Type::Int; }
            INST_INSERT(op1, {}, des, Operator::def, def);
        }
    }
}

// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
// FuncDef.t 
// FuncDef.n **
void frontend::Analyzer::analyseFuncDef(FuncDef* root){
    // 分析FuncType
    ANALYSIS(functype, FuncType, 0);
    // 向上传值
    ir::Type func_type = functype->t;
    std::string func_name = ((Term*) root->children[1])->token.value;
    root->t = func_type;
    root->n = func_name;
    // 声明函数初始化
    Function* func = new Function(func_name, func_type);
    Block* block;
    // 如果为main函数，在main函数执行前，需要先调用全局变量初始化函数
    if(func_name == "main"){
        // 无参函数
        ir::CallInst* Global_Call = new ir::CallInst(ir::Operand("global",ir::Type::null), ir::Operand("t",ir::Type::null));
        func->addInst(Global_Call);
    }
    // 加入函数表，并将当前函数指针指向func
    symbol_table.functions[func_name] = func;
    curr_function = func;
    // 判断是否是无参函数
    bool have_para = root->children.size() == 6;
    if(have_para){
        analyseFuncFParams((FuncFParams*) root->children[3]);
        block = GET_CHILD(Block, 5);
    }
    else{
        block = GET_CHILD(Block, 4);
    }
    // 声明这个函数之后会进入到一个新的作用域
    symbol_table.add_scope(block);
    // 分析Block
    ANALYSIS_WITH_NODE(block, Block);
    // 退出作用域
    symbol_table.exit_scope();
    // 根据函数的返回值类型，判断应该返回什么值
    if(func_type == ir::Type::null){
        // 无返回值
        INST_INSERT(Operand(), Operand(), Operand(), Operator::_return, _return);
    }
    else{
        // 有返回值，仅考虑整数
        INST_INSERT(Operand("int", Type::IntLiteral), Operand(), Operand(), Operator::_return, _return);
    }
    // 退出作用域后需更改当前指针, 并向程序体中添加该函数
    curr_function = nullptr;
    program.addFunction(*func);
}

// FuncType -> 'void' | 'int' | 'float'
// FuncType.t
void frontend::Analyzer::analyseFuncType(FuncType* root){
    Term* child = GET_CHILD(Term, 0);
    // 判断返回值类型
    if(child->token.type == TokenType::VOIDTK){ root->t = ir::Type::null; }
    else{ root->t = ir::Type::Int; }
}

// FuncFParam -> BType Ident ['[' ']' { '[' Exp ']' }] **
void frontend::Analyzer::analyseFuncFParam(FuncFParam* root){
    // 分析BType
    ANALYSIS(btype, BType, 0);
    // 分析Ident
    Term* ident = GET_CHILD(Term, 1);
    // 判断是否是数组
    bool is_array = root->children.size() > 2;
    // EXP的位置
    int default_pos = 5;
    // ste初始化
    STE ste;
    if(is_array) {
        ste.dimension.push_back(0);
        while(default_pos < (int) root->children.size()) {
            ANALYSIS(exp, Exp, default_pos);
            ste.dimension.push_back(std::stoi(exp->v));
            default_pos += 3;
        }
        ste.operand = Operand(ident->token.value, Type::IntPtr);
        insert_ste(ste.operand.name, ste);
        Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
        curr_function->ParameterList.push_back(op1);
    }
    else{
        ste.operand = Operand(ident->token.value, is_array ? Type::IntPtr : btype->t);
        insert_ste(ste.operand.name, ste);
        Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
        curr_function->ParameterList.push_back(op1);
    }
}

// FuncFParams -> FuncFParam { ',' FuncFParam } **
void frontend::Analyzer::analyseFuncFParams(FuncFParams* root){
    // 分析第一个FuncFParam
    ANALYSIS(funcfparam, FuncFParam, 0);
    // 分析剩余的FuncFParam
    int default_pos = 2;
    while(default_pos < (int) root->children.size()){
        ANALYSIS(funcfparam, FuncFParam, default_pos);
        default_pos += 2;
    }
}

// Block -> '{' { BlockItem } '}' **
void frontend::Analyzer::analyseBlock(Block* root){
    // 分析所有的BlockItem
    int default_pos = 1;
    while(default_pos < (int) root->children.size() - 1){
        ANALYSIS(blockitem, BlockItem, default_pos);
        default_pos++;
    }
}

// BlockItem -> Decl | Stmt **
void frontend::Analyzer::analyseBlockItem(BlockItem* root){
    if(root->children[0]->type == NodeType::DECL) { ANALYSIS(decl, Decl, 0); }
    else { ANALYSIS(stmt, Stmt, 0); }
}

// Stmt -> 
// LVal '=' Exp ';' |
// Block |
// 'if' '(' Cond ')' Stmt [ 'else' Stmt ] |
// 'while' '(' Cond ')' Stmt |
// 'break' ';' |
// 'continue' ';' |
// 'return' [Exp] ';' |
// [Exp] ';' **
void frontend::Analyzer::analyseStmt(Stmt* root){
    // 根据第一个孩子判断即可
    auto child = root->children[0];
    auto child_type = child->type;
    // LVal '=' Exp ';'
    if(child_type == NodeType::LVAL){
        LVal* lval = (LVal*) child;
        analyseLVal(lval, 0);
        ANALYSIS(exp, Exp, 2);
        // 添加指令， 没有考虑指针的浮点型情况
        Operand op1 = Operand(exp->v, exp->t);
        Operand des = Operand(symbol_table.get_scoped_name(lval->v), lval->t);
        INST_INSERT(op1,Operand(), des, Operator::mov, mov);
    }
    else if(child_type == NodeType::BLOCK){
        Block* block = (Block*) child;
        // 进入新的作用域
        symbol_table.add_scope(block);
        // 分析Block
        ANALYSIS_WITH_NODE(block, Block);
        // 退出作用域
        symbol_table.exit_scope();
    }
    // [EXP];
    else if(child_type == NodeType::EXP){
        ANALYSIS(exp, Exp, 0);
    }
    // 之后全是终结符的情况
    else{
        // 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
        if(((Term*) child)->token.type == TokenType::IFTK){
            // 分析Cond
            ANALYSIS(cond, Cond, 2);
            // 添加跳转指令，方便短路运算
            // Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            // INST_INSERT(Operand(), Operand(), des, Operator::_goto);
            // 分析Stmt
            ANALYSIS(stmt, Stmt, 4);
            // 如果有else
            if(root->children.size() == 7){
                // 添加跳转指令
                // Operand des2 = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
                // INST_INSERT(Operand(), Operand(), des2, Operator::_goto);
                // 分析Stmt
                ANALYSIS(stmt, Stmt, 6);
            }
        }
        // 'while' '(' Cond ')' Stmt
        else if(((Term*) child)->token.type == TokenType::WHILETK){
            // 分析Cond
            ANALYSIS(cond, Cond, 2);
            // 分析Stmt
            ANALYSIS(stmt, Stmt, 4);
        }
        // 'break' ';'
        else if(((Term*) child)->token.type == TokenType::BREAKTK){
            // 添加指令
            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            INST_INSERT(Operand(), Operand(), des, Operator::_goto, _goto);
        }
        // 'continue' ';'
        else if(((Term*) child)->token.type == TokenType::CONTINUETK){
            // 添加指令
            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            INST_INSERT(Operand(), Operand(), des, Operator::_goto, _goto);
        }
        // 'return' [Exp] ';'
        else if(((Term*) child)->token.type == TokenType::RETURNTK){
            // 分析Exp
            // Exp -> AddExp
            if(root->children.size() == 3){
                ANALYSIS(exp, Exp, 1);
                // 添加指令/仅考虑整型
                INST_INSERT(Operand(exp->v, Type::Int), Operand(), Operand(), Operator::_return, _return);
            }
            else{
                // 添加指令/直接返回
                INST_INSERT(Operand(), Operand(), Operand(), Operator::_return, _return);
            }
        }
        // ';'
        else{
            // do nothing
        }
    }
}

// Exp -> AddExp
// Exp.is_computable
// Exp.v
// Exp.t
void frontend::Analyzer::analyseExp(Exp* root){
    // 分析AddExp
    ANALYSIS(addexp, AddExp, 0);
    // 向上传值
    COPY_EXP_NODE(addexp, root);
}

// Cond -> LOrExp
// Cond.is_computable
// Cond.v
// Cond.t
void frontend::Analyzer::analyseCond(Cond* root){
    // 分析LOrExp
    ANALYSIS(lorexp, LOrExp, 0);
    // 向上传值
    COPY_EXP_NODE(lorexp, root);
}

// UnaryOp -> '+' | '-' | '!'
// UnaryOp.op
void frontend::Analyzer::analyseUnaryOp(UnaryOp* root){
    // 传递值
    root->op = ((Term*)root->children[0])->token.type;
}


// LVal -> Ident {'[' Exp ']'}
// LVal.is_computable
// LVal.v
// LVal.t
// LVal.i   ???
void frontend::Analyzer::analyseLVal(LVal* root, int need){
    Term* term = GET_CHILD(Term, 0);
    // 向上传值
    root->v = term->token.value;
    root->t = Type::Int;

}