#include"front/semantic.h"
#include<exception>
#include<cassert>

/* 本实验将放弃Float类型的支持 */

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;
using ir::CallInst;

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
                                                Instruction* instname = new Instruction(op, {}, opname, Operator::mov); \
                                                INSERT(instname);
// 指令插入
#define INST_INSERT(op1, op2, des, op, name) auto name = new Instruction(op1, op2, des, op); \
                                             if (curr_function == nullptr) \
                                             g_init_inst.push_back(name); \
                                             else \
                                             curr_function->addInst(name);
                                             
#define INSERT(inst) if(curr_function == nullptr) \
                     g_init_inst.push_back(inst); \
                     else \
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
    return {};
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
    globalFunc->addInst(new Instruction({}, {}, {}, Operator::_return));
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
        for(; index < init_index + res; index++) {
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
            Operand op3 = Operand("0", Type::IntLiteral);
            ASSIGN_INT_TO_TMP(op3, des, mov);
            INST_INSERT(op1, op2, des, Operator::store, store);
        }
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
        INST_INSERT({}, {}, {}, Operator::_return, _return);
    }
    else{
        // 有返回值，仅考虑整数
        INST_INSERT(Operand("int", Type::IntLiteral), {}, {}, Operator::_return, _return);
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
// [Exp] ';'
void frontend::Analyzer::analyseStmt(Stmt* root){
    // 根据第一个孩子判断即可
    auto child = root->children[0];
    auto child_type = child->type;
    // LVal '=' Exp ';'
    if(child_type == NodeType::LVAL){
        ANALYSIS(lval, LVal, 0);
        ANALYSIS(exp, Exp, 2);
        // 如果左值为数组
        if(lval->i != ""){
            Operand op1 = Operand(symbol_table.get_scoped_name(lval->v), Type::IntPtr);
            Operand op2 = Operand(lval->i, Type::Int);
            Operand des = Operand(exp->v, exp->t);
            INST_INSERT(op1, op2, des, Operator::store, store);
        }
        else{
            Operand op1 = Operand(exp->v, exp->t);
            Operand des = Operand(symbol_table.get_scoped_name(lval->v), lval->t);
            INST_INSERT(op1, {}, des, Operator::mov, mov);
        }
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
    else if(child_type == NodeType::EXP){ ANALYSIS(exp, Exp, 0); }
    // 之后全是终结符的情况
    else{
        // 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
        if(((Term*) child)->token.type == TokenType::IFTK){
            // calc cond
            Cond* cond = (Cond*) root->children[2];
            curr_cond = cond;
            analyseCond(cond);
            curr_cond = nullptr;
            Instruction* elseinst = nullptr;
            Instruction* breakelseinst = nullptr;

            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            elseinst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(elseinst);
            // goto [pc, else]

            // process goto_in
            for(Instruction *inst : cond->jump_in) {
                inst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(inst->des.name));
            }
            analyseStmt((Stmt*) root->children[4]);
            if(root->children.size() >= 6) {
                // end of if
                Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
                breakelseinst = new Instruction({}, {}, des, Operator::_goto);
                INSERT(breakelseinst);
            }
            elseinst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(elseinst->des.name));
            if(root->children.size() >= 6) {
                analyseStmt((Stmt*) root->children[6]);
                breakelseinst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(breakelseinst->des.name));
            }
        }
        // 'while' '(' Cond ')' Stmt
        else if(((Term*) child)->token.type == TokenType::WHILETK){
            curr_while_stmt.push_back(root);
            Cond* cond = (Cond*) root->children[2];
            curr_cond = cond;
            int condpos = curr_function->InstVec.size();
            analyseCond(cond);
            curr_cond = nullptr;
            Instruction* elseinst = nullptr;
            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            elseinst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(elseinst);
            // goto [pc, else]
            for(Instruction *inst : cond->jump_in) {
                inst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(inst->des.name));
            }
            analyseStmt((Stmt*) root->children[4]);
            Operand des2 = Operand(std::to_string(condpos - (int) curr_function->InstVec.size()), Type::IntLiteral);
            INST_INSERT({}, {}, des2, Operator::_goto, _goto);
            elseinst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(elseinst->des.name));
            for(Instruction* inst : root->jump_eow) {
                inst->des.name = std::to_string(curr_function->InstVec.size() - std::stoi(inst->des.name));
            }
            for(Instruction* inst : root->jump_bow) {
                inst->des.name = std::to_string(condpos - std::stoi(inst->des.name));
            }
            // end of while
            curr_while_stmt.pop_back();
        }
        // 'break' ';'
        else if(((Term*) child)->token.type == TokenType::BREAKTK){
            // 添加指令
            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            Instruction* goto_inst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(goto_inst);
            // 添加当前循环的跳转指令
            curr_while_stmt.back()->jump_eow.insert(goto_inst);
        }
        // 'continue' ';'
        else if(((Term*) child)->token.type == TokenType::CONTINUETK){
            // 添加指令
            Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            Instruction* goto_inst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(goto_inst);
            // 添加当前循环的跳转指令
            curr_while_stmt.back()->jump_bow.insert(goto_inst);
        }
        // 'return' [Exp] ';'
        else if(((Term*) child)->token.type == TokenType::RETURNTK){
            // 分析Exp
            // Exp -> AddExp
            if(root->children.size() == 3){
                ANALYSIS(exp, Exp, 1);
                if(exp->t == Type::IntLiteral){
                    INST_INSERT(Operand(exp->v, exp->t), {}, {}, Operator::_return, _return);
                }
                else{
                    INST_INSERT(Operand(exp->v, Type::Int), {}, {}, Operator::_return, _return);
                }
            }
            else{
                // 添加指令/直接返回
                INST_INSERT({}, {}, {}, Operator::_return, _return);
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



// LVal -> Ident {'[' Exp ']'}
// LVal.is_computable
// LVal.v
// LVal.t
// LVal.i   changed
void frontend::Analyzer::analyseLVal(LVal* root){
    // 分析Ident
    Term* ident = GET_CHILD(Term, 0);
    // 判断是否为数组
    bool is_array = root->children.size() > 1;
    // 由于需要赋值，需要将之前声明的变量拿出来
    STE ste = symbol_table.get_ste(ident->token.value);
    root->v = symbol_table.get_scoped_name(ident->token.value);
    root->t = Type::Int;

    if(is_array) { 
        // 生成临时变量des  
        ASSIGN_INT_TO_TMP(Operand("0", Type::IntLiteral), des, movToDes);
        // ident [exp1] [exp2]
        for(int i = 2; i < (int) root->children.size(); i += 3) {
            // 生成临时变量tmp，该临时变量的值为当前维度的下标
            ANALYSIS(exp, Exp, i);
            ASSIGN_INT_TO_TMP(Operand(exp->v, exp->t), tmp, movToTmp);
            // des = des * dimension[i / 3] + tmp
            INST_INSERT(des, tmp, des, Operator::add, add);
            if(i + 3 < (int) root->children.size()) {
                INST_INSERT(Operand(std::to_string(ste.dimension[(i + 1) / 3]), Type::IntLiteral), {}, tmp, Operator::mov, mov2);
                INST_INSERT(des, tmp, des, Operator::mul, mul);
            }
        }
        // 由于Lval可能参与运算，所以需要将其值存放到临时变量中
        // 根据语法树的结构，如果LVal的父节点为PrimaryExp，就需要加载
        // 如果LVal的父节点为Stmt，则可能需要为其赋值，所以不需要加载
        if(root->parent->type == NodeType::PRIMARYEXP){ 
            if(ste.operand.type == Type::IntPtr) {
                // load值存放临时位置
                Operand load_data = Operand(GET_TMP_NAME, Type::Int);
                // load数组
                Operand op1 = Operand(symbol_table.get_scoped_name(ident->token.value), Type::IntPtr);
                // load下标即为des
                INST_INSERT(op1, des, load_data, Operator::load, load);
                root->v = load_data.name;
            }
        }
        // LVal的i属性为数组的下标，方便赋值或者取值
        root->i = des.name;
    }
    // 非数组情况
    else {
        root->v = ste.operand.name;
        if(ste.operand.type == Type::IntLiteral) {  root->is_computable = true; root->t = Type::IntLiteral;
        } else if(ste.operand.type == Type::Int) { root->is_computable = false; root->t = Type::Int;
        } else if(ste.operand.type == Type::IntPtr) { root->is_computable = false; root->t = Type::IntPtr;
        }  
    }
}

// Number -> IntConst | floatConst
void frontend::Analyzer::analyseNumber(Number* root){
    Term* term = GET_CHILD(Term, 0);
    root->v = term->token.value;
    root->t = Type::FloatLiteral;
    // 如果是整数常量
    if(term->token.type == TokenType::INTLTR){
        root->t = Type::IntLiteral;
        int value = 0;
        if(root->v[0] == '0' && root->v[1] == 'b') { value = std::stoul(root->v, nullptr, 2);
        } else if(root->v[0] == '0' && root->v[1] == 'x') { value = std::stoul(root->v, nullptr, 16);
        } else if(root->v[0] == '0' && root->v.length() > 1) { value = std::stoul(root->v, nullptr, 8);
        } else { value = std::stoul(root->v, nullptr, 10);
        }
        root->v = std::to_string(value);
    }
}

// PrimaryExp -> '(' Exp ')' | LVal | Number
// LVal -> Ident {'[' Exp ']'}
// PrimaryExp.is_computable
// PrimaryExp.v
// PrimaryExp.t
void frontend::Analyzer::analysePrimaryExp(PrimaryExp* root){
    // 根据第一个子节点的类型进行分析
    auto child = root->children[0];
    auto child_type = child->type;
    // '(' Exp ')'
    if(child_type == NodeType::TERMINAL){
        ANALYSIS(exp, Exp, 1);
        COPY_EXP_NODE(exp, root);
    }
    // Lval有以下情况：Int，IntPtr，IntLiteral
    else if(child_type == NodeType::LVAL){
        ANALYSIS(lval, LVal, 0);
        if(lval->t == Type::Int){
            Operand op1 = Operand(symbol_table.get_scoped_name(lval->v), Type::Int);
            ASSIGN_INT_TO_TMP(op1, des, movToDes);
            root->v = des.name;
            root->t = Type::Int;
            root->is_computable = false;
        }
        else{ COPY_EXP_NODE(lval, root); }
    }
    else{
        ANALYSIS(number, Number, 0);
        COPY_EXP_NODE(number, root);
    }
}

// UnaryExp -> 
// PrimaryExp | 
// Ident '(' [FuncRParams] ')' | 
// UnaryOp UnaryExp
// UnaryExp.is_computable
// UnaryExp.v
// UnaryExp.t
void frontend::Analyzer::analyseUnaryExp(UnaryExp* root){
    auto child = root->children[0];
    auto child_type = child->type;
    if(child_type == NodeType::PRIMARYEXP){
        ANALYSIS(primaryExp, PrimaryExp, 0);
        COPY_EXP_NODE(primaryExp, root);
    }
    // 函数调用 Ident '(' [FuncRParams] ')'
    else if(child_type == NodeType::TERMINAL){
        // 所调用函数名
        std::string funcname = GET_CHILD(Term, 0)->token.value;
        // 从作用域中找到函数
        // 由于初始化的时候已经将静态函数库放到全局函数中，所以不用单独查询
        Function* func = symbol_table.functions[funcname];
        // 无参函数 Ident '(' ')'
        if(root->children[2]->type == NodeType::TERMINAL){
            // 生成函数调用指令
            Operand op1 = Operand(funcname, func->returnType);
            Operand des = Operand(GET_TMP_NAME, func->returnType);
            CallInst* call = new CallInst(op1, des);
            INSERT(call);
            root->v = des.name;
        }
        // 有参函数 Ident '(' FuncRParams ')'
        else{
            FuncRParams* funcRParams = GET_CHILD(FuncRParams, 2);
            std::vector<Operand> paras = analyseFuncRParams(funcRParams);
            // 生成函数调用指令
            Operand op1 = Operand(funcname, func->returnType);
            Operand des = Operand(GET_TMP_NAME, func->returnType);
            CallInst* call = new CallInst(op1, paras, des);
            INSERT(call);
            root->v = des.name;
        }
        root->t = func->returnType;
        root->is_computable = false;
    }
    // UnaryOp UnaryExp
    else{
        ANALYSIS(unaryop, UnaryOp, 0);
        ANALYSIS(unaryexp, UnaryExp, 1);
        COPY_EXP_NODE(unaryexp, root);
        // 对于常数来说，直接计算结果
        if(unaryexp->is_computable){
            if (unaryop->op == TokenType::NOT) {
                root->v = root->v == "0" ? "1" : "0";
                root->t = Type::IntLiteral;
            }
            else if(unaryop->op == TokenType::MINU){
                root->v = "-" + root->v;
            }
        }
        // 对于变量来说，生成指令
        else{
            if(unaryop->op == TokenType::MINU){
                // des = 0 - des
                ASSIGN_INT_TO_TMP(Operand("0", Type::IntLiteral), zero, mov);
                Operand des = Operand(root->v, root->t);
                INST_INSERT(zero, des, des, Operator::sub, sub);
            }
            else if(unaryop->op == TokenType::NOT){
                Operand des = Operand(root->v, root->t);
                INST_INSERT(des, {}, des, Operator::_not, _not);
            }
        }
    }
}

// UnaryOp -> '+' | '-' | '!'
void frontend::Analyzer::analyseUnaryOp(UnaryOp* root) {
    root->op = GET_CHILD(Term, 0)->token.type;
}

// FuncRParams -> Exp { ',' Exp }
std::vector<ir::Operand> frontend::Analyzer::analyseFuncRParams(FuncRParams* root){
    std::vector<ir::Operand> paras;
    for(int i = 0; i < (int) root->children.size(); i += 2){
        ANALYSIS(exp, Exp, i);
        Operand op1 = Operand(symbol_table.get_scoped_name(exp->v), exp->t);
        paras.push_back(op1);
    }
    return paras;
}

void frontend::Analyzer::analyseLOrExp(LOrExp* root) {
    ANALYSIS(landexp, LAndExp, 0)
    if(root->children.size() > 1) { ANALYSIS(lorexp, LOrExp, 2) }
    else{ COPY_EXP_NODE(landexp, root); }
}

void frontend::Analyzer::analyseConstExp(ConstExp* root) {
    ANALYSIS(addexp, AddExp, 0)
    COPY_EXP_NODE(addexp, root);
}

// MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
// MulExp.is_computable
// MulExp.v
// MulExp.t
void frontend::Analyzer::analyseMulExp(MulExp* root) {
    if(root->children.size() == 1) { // 只有一个儿子 直接继承
        ANALYSIS(unaryexp, UnaryExp, 0);
        COPY_EXP_NODE(unaryexp, root);
    }
    else{
        root->is_computable = true;
        for(int i = 0; i < (int) root->children.size(); i += 2) {
            ANALYSIS(unaryexp,UnaryExp, i);
            // 如果有一个不可计算，那么整个表达式都不可计算
            if(!unaryexp->is_computable) { root->is_computable = false; }
        }
        if(root->is_computable) {
            int res = std::stoi(GET_CHILD(UnaryExp, 0)->v);
            for(int i = 2; i < (int) root->children.size(); i += 2){
                int tmp = std::stoi(GET_CHILD(UnaryExp, i)->v);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::MULT) { res *= tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::DIV) { res /= tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::MOD) { res %= tmp; }
                }
            }
            root->v = std::to_string(res);
            root->t = Type::IntLiteral;
        } else {
            // 生成指令
            // 临时变量存储最终结果
            Operand op1 = Operand(GET_CHILD(UnaryExp, 0)->v, GET_CHILD(UnaryExp, 0)->t);
            ASSIGN_INT_TO_TMP(op1, res, mov);
            Operator op;
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                UnaryExp* unaryexp = GET_CHILD(UnaryExp, i);
                Term* term = GET_CHILD(Term, i - 1);
                Operand op3 = Operand(unaryexp->v, unaryexp->t);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::MULT) { op = Operator::mul; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::DIV) { op = Operator::div; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::MOD) { op = Operator::mod; }
                }
                if(unaryexp->is_computable) {
                    Operand op2 = Operand(GET_TMP_NAME, Type::Int);
                    curr_function->addInst(new Instruction(op3, {}, op2, Operator::mov));
                    // ASSIGN_INT_TO_TMP(op3, op2, mov)
                    INST_INSERT(res, op2, res, op, oper);
                } else {
                    INST_INSERT(res, op3, res, op, oper);
                }
            }
            root->t = Type::Int;
            root->v = res.name;
        }
    }
     
}

// AddExp -> MulExp { ('+' | '-') MulExp }
// AddExp.is_computable
// AddExp.v
// AddExp.t
void frontend::Analyzer::analyseAddExp(AddExp* root){
    if(root->children.size() == 1) { // 只有一个儿子 直接继承
        ANALYSIS(mulexp, MulExp, 0);
        COPY_EXP_NODE(mulexp, root);
    }
    else{
        // 分析所有的MulExp
        root->is_computable = true;
        for(int i = 0; i < (int) root->children.size(); i += 2){
            ANALYSIS(mulexp, MulExp, i);
            // 如果有一个不可计算，那么整个表达式都不可计算
            if(!mulexp->is_computable) { root->is_computable = false; }
        }
        // 如果所有的都可计算，那么计算结果
        if(root->is_computable){
            int res = std::stoi(GET_CHILD(MulExp, 0)->v);
            for(int i = 2; i < (int) root->children.size(); i += 2){
                int tmp = std::stoi(GET_CHILD(MulExp, i)->v);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::PLUS) { res += tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::MINU) { res -= tmp; }
                }
            }
            root->v = std::to_string(res);
            root->t = Type::IntLiteral;
        }
        // 如果是变量，并将结果存入临时变量
        else{
            // 生成指令
            // 临时变量存储最终结果
            Operand op1 = Operand(GET_CHILD(MulExp, 0)->v, GET_CHILD(MulExp, 0)->t);
            ASSIGN_INT_TO_TMP(op1, res, mov);
            Operator op;
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                MulExp* mulexp = GET_CHILD(MulExp, i);
                Term* term = GET_CHILD(Term, i - 1);
                Operand op3 = Operand(mulexp->v, mulexp->t);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::PLUS) { op = Operator::add; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::MINU) { op = Operator::sub; }
                }
                if(mulexp->is_computable) {
                    Operand op2 = Operand(GET_TMP_NAME, Type::Int);
                    curr_function->addInst(new Instruction(op3, {}, op2, Operator::mov));
                    INST_INSERT(res, op2, res, op, oper);
                } else {
                    INST_INSERT(res, op3, res, op, oper);
                }
            }
            root->t = Type::Int;
            root->v = res.name;
        }
    }
}


// RelExp -> AddExp { ('<' | '>' | '<=' | '>=') AddExp }
// RelExp.is_computable
// RelExp.v
// RelExp.t
void frontend::Analyzer::analyseRelExp(RelExp* root) {
    if(root->children.size() == 1) { // 只有一个儿子 直接继承
        ANALYSIS(addexp, AddExp, 0);
        COPY_EXP_NODE(addexp, root);
    }
    else{
        root->is_computable = true;
        for(int i = 0; i < (int) root->children.size(); i += 2) {
            ANALYSIS(addexp, AddExp, i);
            if(!addexp->is_computable) { root->is_computable = false; }
        }
        if(root->is_computable) {
            int res = std::stoi(GET_CHILD(AddExp, 0)->v);
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                int tmp = std::stoi(GET_CHILD(AddExp, i)->v);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::LSS) { res = res < tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::LEQ) { res = res <= tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::GTR) { res = res > tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::GEQ) { res = res >= tmp; }
                }
            }
            root->v = std::to_string(res);
            root->t = Type::IntLiteral;
        } else {
            // 生成指令
            // 临时变量存储最终结果
            Operand op1 = Operand(GET_CHILD(AddExp, 0)->v, GET_CHILD(AddExp, 0)->t);
            ASSIGN_INT_TO_TMP(op1, des, mov);
            Operator op;
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                AddExp* addexp = GET_CHILD(AddExp, i);
                Term* term = GET_CHILD(Term, i - 1);
                Operand op3 = Operand(addexp->v, addexp->t);  
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::LSS) { op = Operator::lss; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::LEQ) { op = Operator::leq; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::GTR) { op = Operator::gtr; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::GEQ) { op = Operator::geq; }
                }
                if(addexp->is_computable) {
                    ASSIGN_INT_TO_TMP(op3, op2, mov);
                    INST_INSERT(des, op2, des, op, oper);
                } else {
                    INST_INSERT(des, op3, des, op, oper);
                }
            }
            root->v = des.name;
            root->t = Type::Int;
        }
    }
}

// EqExp -> RelExp { ('==' | '!=') RelExp }
// EqExp.is_computable
// EqExp.v
// EqExp.t
void frontend::Analyzer::analyseEqExp(EqExp* root) {
    if(root->children.size() == 1) { // 只有一个儿子 直接继承
        ANALYSIS(relexp, RelExp, 0);
        COPY_EXP_NODE(relexp, root);
    }
    else{
        root->is_computable = true;
        
        for(int i = 0; i < (int) root->children.size(); i += 2) {
            ANALYSIS(relexp, RelExp, i);
            if(!relexp->is_computable) { root->is_computable = false; }
        }
        if(root->is_computable) {
            int res = std::stoi(((RelExp*) root->children[0])->v);
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                int tmp = std::stoi(((RelExp*) root->children[i])->v);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::EQL) { res = res == tmp; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::NEQ) { res = res != tmp; }
                }
            }
            root->v = std::to_string(res);
            root->t = Type::IntLiteral;
        } else {
            // 生成指令
            // 临时变量存储最终结果
            Operand op1 = Operand(((RelExp*) root->children[0])->v, ((RelExp*) root->children[0])->t);
            ASSIGN_INT_TO_TMP(op1, des, mov);
            Operator op;
            for(int i = 2; i < (int) root->children.size(); i += 2) {
                RelExp* relexp = GET_CHILD(RelExp, i);
                Term* term = GET_CHILD(Term, i - 1);
                Operand op3 = Operand(relexp->v, relexp->t);
                if(root->children[i - 1]->type == NodeType::TERMINAL){
                    if(GET_CHILD(Term, i - 1)->token.type == TokenType::EQL) { op = Operator::eq; }
                    else if(GET_CHILD(Term, i - 1)->token.type == TokenType::NEQ) { op = Operator::neq; }
                }
                if(relexp->is_computable) {
                    ASSIGN_INT_TO_TMP(op3, op2, mov);
                    INST_INSERT(des, op2, des, op, oper);
                } else {
                    INST_INSERT(des, op3, des, op, oper);
                }
            }
            root->v = des.name;
            root->t = Type::Int;
        }
    }
}

// LAndExp -> EqExp [ '&&' LAndExp ]
// LAndExp.is_computable
// LAndExp.v 
// LAndExp.t 
void frontend::Analyzer::analyseLAndExp(LAndExp* root){
    ANALYSIS(eqexp, EqExp, 0);
    if(eqexp->is_computable) {
        if(!std::stoi(eqexp->v)) {
            COPY_EXP_NODE(eqexp, root);
        } else {
            if(root->children.size() > 1) {
                ANALYSIS(landexp, LAndExp, 2);
                COPY_EXP_NODE(landexp, root);
            } else {
                Operand des = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
                Instruction* inst = new Instruction({}, {}, des, Operator::_goto);
                curr_cond->jump_in.insert(inst);
                INSERT(inst);
                COPY_EXP_NODE(eqexp, root);
            }
        } 
    } 
    else {
        // 首先判断eqexp是否为0，结果存放在des中
        Operand des;

        Operand op1 = Operand(eqexp->v, eqexp->t);
        Operand op2 = Operand("0", Type::IntLiteral);
        des = Operand(eqexp->v, eqexp->t);
        INST_INSERT(op1, op2, des, Operator::eq, eq);

        int pos = curr_function->InstVec.size();
        // 如果eqexp为1，直接跳转到下一个or
        Instruction *goto_next_or = new Instruction(des, {}, Operand("0", Type::IntLiteral), Operator::_goto);
        INSERT(goto_next_or);
        if(root->children.size() > 1) {
            ANALYSIS(landexp, LAndExp, 2);
            COPY_EXP_NODE(landexp, root);
        } else {
            // the last eqexp goto in of if/while
            Operand des2 = Operand(std::to_string(curr_function->InstVec.size()), Type::IntLiteral);
            Instruction *goto_in = new Instruction({}, {}, des2, Operator::_goto);
            curr_cond->jump_in.insert(goto_in);
            INSERT(goto_in);
            COPY_EXP_NODE(eqexp, root);
        }
        // 跳到下一个or，也就是现在的位置
        goto_next_or->des.name = std::to_string(curr_function->InstVec.size() - pos);
    }
}