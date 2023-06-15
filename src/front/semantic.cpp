#include"front/semantic.h"

#include<cassert>
#include<iostream>
#include<string>
#include<iomanip>

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;
using ir::CallInst; 
#define TODO assert(0 && "TODO");
#define ZERO Operand("0", Type::IntLiteral)
#define GET_CHILD(type, index) dynamic_cast<type *>(root->children[index])
#define ANALYSIS(node, type, index) auto node = dynamic_cast<type *>(root->children[index]); analyse##type(node);
#define ANALYSIS_WITH_NODE(node, type) analyse##type((type*)node);
#define COPY_EXP_NODE(from, to) to->is_computable = from->is_computable; to->v = from->v; to->t = from->t;
#define GET_TMP_NAME ("t" + std::to_string(tmp_cnt++))
#define ASSIGN_INT_TO_TMP(op, opname, instname) Operand opname(GET_TMP_NAME, Type::Int); Instruction* instname = new Instruction(op, {}, opname, Operator::mov); INSERT(instname);
#define INST_INSERT(op1, op2, des, op, name) auto name = new Instruction(op1, op2, des, op); if (current_func == nullptr) g_init_inst.push_back(name); else current_func->addInst(name);                             
#define INSERT(inst) if(current_func == nullptr) g_init_inst.push_back(inst); else current_func->addInst(inst);
#define log(...)


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
        {"starttime", new Function("starttime", {}, Type::null)},
        {"stoptime", new Function("stoptime", {}, Type::null)}
    };
    return &lib_funcs;
}

void frontend::SymbolTable::add_scope(Block* node) {
    cnt ++;
    int scopeID = cnt;
    string scope_name = std::to_string(scopeID);
    ScopeInfo scope_info;
    scope_info.name = scope_name;
    scope_stack.push_back(scope_info);
}

void frontend::SymbolTable::exit_scope() {
    scope_stack.pop_back();
}

string frontend::SymbolTable::get_scoped_name(string id) const {
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            return id + "_" + scope_stack[i].name;
        }
    }
    return id;
}

Operand frontend::SymbolTable::get_operand(string id) const {
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            return scope_stack[i].table.at(id).operand;
        }
    }
    return {};
}

frontend::STE frontend::SymbolTable::get_ste(string id) const {
    for(int i = (int) scope_stack.size() - 1; i >= 0; i--){
        if(scope_stack[i].table.find(id) != scope_stack[i].table.end()){
            return scope_stack[i].table.at(id);
        }
    }
    return STE();
}

frontend::Analyzer::Analyzer(): tmp_cnt(0), symbol_table(), current_func(nullptr) {
    symbol_table.cnt = 0;
    symbol_table.scope_stack = vector<frontend::ScopeInfo>();
    symbol_table.functions = *get_lib_funcs();
    ScopeInfo scope_info;
    scope_info.name = "g";
    scope_info.cnt  = symbol_table.cnt;
    symbol_table.scope_stack.push_back(scope_info);
}

ir::Program frontend::Analyzer::get_ir_program(CompUnit* root) {
    analyseCompUnit(root);
    Function* globalFunc = new Function("global", ir::Type::null);
    for(auto i : g_init_inst) globalFunc->addInst(i);
    globalFunc->addInst(new Instruction({}, {}, {}, Operator::_return));
    program.addFunction(*globalFunc);
    program.draw();
    return program;
}



void frontend::Analyzer::insert_ste(std::string name, STE ste) {
    symbol_table.scope_stack.back().table[name] = ste;
    if(current_func == nullptr) {
        ste.operand.name = ste.operand.name + "_g";
        if(ste.operand.type == Type::IntPtr) {
            int len = 1;
            for(int i : ste.dimension) { len *= i; }
            program.globalVal.emplace_back(ste.operand, len);
        } else if(ste.operand.type == Type::Int) { program.globalVal.emplace_back(ste.operand); }
    }
}

// CompUnit -> (Decl | FuncDef) [CompUnit] **
void frontend::Analyzer::analyseCompUnit(CompUnit* root) {
    if(root->children[0]->type == NodeType::DECL){ ANALYSIS(decl, Decl, 0); }
    else{ ANALYSIS(funcdef, FuncDef, 0); }
    if(root->children.size() > 1){ ANALYSIS(compunit, CompUnit, 1); }
}
// Decl -> ConstDecl | VarDecl **
void frontend::Analyzer::analyseDecl(Decl* root) {
    if(root->children[0]->type == NodeType::CONSTDECL){ ANALYSIS(constdecl, ConstDecl, 0); }
    else{ ANALYSIS(vardecl, VarDecl, 0); }
}
// ConstDecl -> 'const' BType ConstDef {',' ConstDef}';' **
void frontend::Analyzer::analyseConstDecl(ConstDecl* root) {
    ANALYSIS(btype, BType, 1);
    root->t = btype->t;
    for(int i = 2; i < (int) root->children.size() - 1; i += 2){
        ConstDef* constdef = GET_CHILD(ConstDef, i);
        constdef->t = root->t;
        ANALYSIS_WITH_NODE(constdef, ConstDef);
    }
}
// BType -> 'int' | 'float' **
void frontend::Analyzer::analyseBType(BType* root) {
    root->t = Type::Int;
}
// ConstDef -> Ident { '[' ConstExp ']' } '=' ConstInitVal **
void frontend::Analyzer::analyseConstDef(ConstDef* root) {
 Term* term = GET_CHILD(Term, 0);
    bool is_array = (GET_CHILD(Term, 1))->token.type == TokenType::LBRACK;
    int default_pos = 2;
    int index = 0;
    vector<int> dimension;
    STE ste;
    if(is_array){
        while(root->children[default_pos]->type == NodeType::CONSTEXP){
            ANALYSIS(constexp, ConstExp, default_pos);
            int dim = std::stoi(constexp->v);
            dimension.push_back(dim);
            ste.len *= dim;
            default_pos += 3;
        }
        ste.dimension = dimension;
        ste.operand = Operand(term->token.value, Type::IntPtr);
        insert_ste(ste.operand.name, ste);
        Operand op1 = Operand(std::to_string(ste.len), Type::IntLiteral);
        Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), Type::IntPtr);
        INST_INSERT(op1, {}, des, Operator::alloc, alloc);
        analyseConstInitVal(index, ste, GET_CHILD(ConstInitVal, default_pos));
    }
    else{
        ste.operand = Operand(term->token.value, root->t == Type::Int? Type::IntLiteral : Type::FloatLiteral);
        analyseConstInitVal(index, ste, GET_CHILD(ConstInitVal, default_pos));
        insert_ste(term->token.value, ste);
    }
}
// ConstInitVal -> ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}' **
void frontend::Analyzer::analyseConstInitVal(int &index, STE& ste, ConstInitVal* root) {
    bool is_array = root->children[0]->type == NodeType::CONSTEXP;
    if(is_array){
        ANALYSIS(constexp, ConstExp, 0);
        if(ste.operand.type == Type::IntLiteral){ ste.operand.name = constexp->v; }
        else{
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), Type::IntPtr);
            Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
            Operand op3 = Operand(constexp->v, Type::IntLiteral);
            ASSIGN_INT_TO_TMP(op3, des, mov);
            INST_INSERT(op1, op2, des, Operator::store, store);
        }
        index++;
    }
    // '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    else{
        int default_pos = 1;
        while(default_pos < (int) root->children.size() - 1){
            ste.len /= ste.dimension[ste.level];
            ste.level++;
            analyseConstInitVal(index, ste, (ConstInitVal*) root->children[default_pos]);
            default_pos += 2;
        }
    }
    ste.level = 0;
    for(auto i : ste.dimension){ste.len *= i;}
}
// VarDecl -> BType VarDef { ',' VarDef } ';' **
void frontend::Analyzer::analyseVarDecl(VarDecl* root) {
    root->t = Type::Int;
    for(int i = 1; i < (int) root->children.size(); i += 2) {
        VarDef* vardef = GET_CHILD(VarDef, i);
        vardef->t = Type::Int;
        ANALYSIS_WITH_NODE(vardef, VarDef);
    }
}
// VarDef -> Ident { '[' ConstExp ']' } [ '=' InitVal ] **
void frontend::Analyzer::analyseVarDef(VarDef* root) {
    Term* term = GET_CHILD(Term, 0);
    bool is_array = root->children.size() > 1 && GET_CHILD(Term, 1)->token.type == TokenType::LBRACK;
    int default_pos = 2;
    STE ste;
    std::vector<int> dimension;
    if(is_array) {
        while(default_pos < (int) root->children.size() && root->children[default_pos]->type == NodeType::CONSTEXP) {
            ANALYSIS(constExp, ConstExp, default_pos);
            int dim = std::stoi(constExp->v);
            dimension.push_back(dim);
            default_pos += 3;
            ste.len *= dim;
        }
        ste.operand = Operand(term->token.value, Type::IntPtr);
        ste.dimension = dimension;
        insert_ste(ste.operand.name, ste);
        if(current_func != nullptr) {
            Operand op1 = Operand(std::to_string(ste.len), Type::IntLiteral);
            Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            INST_INSERT(op1, {}, des, Operator::alloc, alloc);
        }
    } 
    else {
        ste.operand = Operand(term->token.value, Type::Int);
        insert_ste(ste.operand.name, ste);
    }
    if(default_pos < (int) root->children.size()) {
        int index = 0;
        analyseInitVal(index, ste, ste.len, ste.level, (InitVal*) root->children[default_pos]);
    }
    else if(current_func == nullptr) {
        if(!is_array) {
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand des = ZERO;
            INST_INSERT(op1, {}, des, Operator::def, def);
        }
    }
}
// InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}' **
void frontend::Analyzer::analyseInitVal(int& index, STE& ste, int res, int level, InitVal* root) {
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
            Operand op3 = ZERO;
            ASSIGN_INT_TO_TMP(op3, des, mov);
            INST_INSERT(op1, op2, des, Operator::store, store);
        }
    } else {
        ANALYSIS(exp, Exp, 0);
        if(ste.operand.type == Type::IntPtr) {
            Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand op2 = Operand(std::to_string(index), Type::IntLiteral);
            if(exp->is_computable) {
                Operand op3 = Operand(exp->v, Type::IntLiteral);
                ASSIGN_INT_TO_TMP(op3, des, mov);
                INST_INSERT(op1, op2, des, Operator::store, store);
            }
            else {
                Operand des = Operand(exp->v, Type::Int);
                INST_INSERT(op1, op2, des, Operator::store, store);
            }
            index++;
        }
        else {
            Operand des = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
            Operand op1;
            op1.name = exp->v;
            if(exp->is_computable) { op1.type = Type::IntLiteral; }
            else { op1.type = Type::Int; }
            INST_INSERT(op1, {}, des, Operator::def, def);
        }
    }
}
// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block **
void frontend::Analyzer::analyseFuncDef(FuncDef* root) {
    ANALYSIS(functype, FuncType, 0);
    ir::Type func_type = functype->t;
    std::string func_name = ((Term*) root->children[1])->token.value;
    root->t = func_type;
    root->n = func_name;
    Function *func = new Function(func_name, func_type);
    Block* block;
    if(func_name == "main") {
        ir::CallInst* callGlobal = new ir::CallInst(ir::Operand("global",ir::Type::null), ir::Operand("t0",ir::Type::null));
        func->addInst(callGlobal);
    }
    symbol_table.functions[func_name] = func;
    current_func = func;
    bool have_para = root->children.size() == 6;
    if(have_para) {
        block = GET_CHILD(Block, 5);
        symbol_table.add_scope(block);
        ANALYSIS(funcfparams, FuncFParams, 3);
    }
    else{
        block = GET_CHILD(Block, 4);
        symbol_table.add_scope(block);
    }
    analyseBlock(block);
    symbol_table.exit_scope();
    if(func_type == ir::Type::null){ INST_INSERT({}, {}, {}, Operator::_return, _return); }
    else if (func_type == ir::Type::Int) { INST_INSERT(ZERO, {}, {}, Operator::_return, _return); }
    else { INST_INSERT(Operand("0", Type::FloatLiteral), {}, {}, Operator::_return, _return); }
    current_func = nullptr;
    program.addFunction(*func);
}
// FuncType -> 'void' | 'int' | 'float' **
void frontend::Analyzer::analyseFuncType(FuncType* root) {
    Term* child = GET_CHILD(Term, 0);
    if(child->token.type == TokenType::VOIDTK){ root->t = ir::Type::null; }
    else{ root->t = ir::Type::Int; }
}
// FuncFParam -> BType Ident ['[' ']' { '[' Exp ']' }] **
void frontend::Analyzer::analyseFuncFParam(FuncFParam* root) {
    ANALYSIS(btype, BType, 0);
    Term* ident = GET_CHILD(Term, 1);
    bool is_array = root->children.size() > 2;
    int default_pos = 5;
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
        current_func->ParameterList.push_back(op1);
    }
    else{
        ste.operand = Operand(ident->token.value, is_array ? Type::IntPtr : btype->t);
        insert_ste(ste.operand.name, ste);
        Operand op1 = Operand(symbol_table.get_scoped_name(ste.operand.name), ste.operand.type);
        current_func->ParameterList.push_back(op1);
    }
}
// FuncFParams -> FuncFParam { ',' FuncFParam } **
void frontend::Analyzer::analyseFuncFParams(FuncFParams* root) {
    ANALYSIS(funcfparam, FuncFParam, 0);
    int default_pos = 2;
    while(default_pos < (int) root->children.size()){
        ANALYSIS(funcfparam, FuncFParam, default_pos);
        default_pos += 2;
    }
}
// Block -> '{' { BlockItem } '}' **
void frontend::Analyzer::analyseBlock(Block* root) {
    int default_pos = 1;
    while(default_pos < (int) root->children.size() - 1){
        ANALYSIS(blockitem, BlockItem, default_pos);
        default_pos++;
    }
}
// BlockItem -> Decl | Stmt **
void frontend::Analyzer::analyseBlockItem(BlockItem* root) {
    if(root->children[0]->type == NodeType::DECL) { ANALYSIS(decl, Decl, 0); }
    else { ANALYSIS(stmt, Stmt, 0); }
}
// Stmt -> LVal '=' Exp ';' | Exp ';' | 'if' '(' Exp ')' Stmt ['else' Stmt] | 'while' '(' Exp ')' Stmt | 'break' ';' | 'continue' ';' | 'return' [Exp] ';' | Block **
void frontend::Analyzer::analyseStmt(Stmt* root) {
    auto child = root->children[0];
    auto child_type = child->type;
    if(child_type == NodeType::BLOCK) {
        Block* block = (Block*) child;
        symbol_table.add_scope(block);
        ANALYSIS_WITH_NODE(block, Block);
        symbol_table.exit_scope();
    }
    else if(root->children[0]->type == NodeType::EXP) {
        ANALYSIS(exp, Exp, 0);
    }
    else if(root->children[0]->type == NodeType::LVAL) {
        ANALYSIS(lval, LVal, 0);
        ANALYSIS(exp, Exp, 2);
        if(lval->i != "") {
            Operand op1 = Operand(symbol_table.get_scoped_name(lval->v), lval->t == Type::Int ? Type::IntPtr : Type::FloatPtr);
            Operand op2 = Operand(lval->i, Type::Int);
            Operand op3 = Operand(exp->v, exp->t);
            if(lval->t != exp->t){
                ASSIGN_INT_TO_TMP(op3, des, mov);
                INST_INSERT(op1, op2, des, Operator::store, store);
            }
            else{
                INST_INSERT(op1, op2, op3, Operator::store, store);
            }
        }
        else {
            Operand op1 = Operand(exp->v, exp->t);
            Operand des = Operand(symbol_table.get_scoped_name(lval->v), lval->t);
            INST_INSERT(op1, {}, des, Operator::mov, mov);
        }
    }
    else{
        if(((Term*) child)->token.type == TokenType::BREAKTK) {
            Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
            Instruction* goto_inst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(goto_inst);
            curr_while_stmt.back()->jump_eow.insert(goto_inst);
        }
        else if(((Term*) child)->token.type == TokenType::CONTINUETK) {
            Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
            Instruction* goto_inst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(goto_inst);
            curr_while_stmt.back()->jump_bow.insert(goto_inst);
        }
        else if(((Term*) child)->token.type == TokenType::IFTK) {
            Cond* cond = GET_CHILD(Cond, 2);
            curr_cond = cond;
            ANALYSIS_WITH_NODE(cond, Cond);
            curr_cond = nullptr;
            Instruction* elseinst = nullptr;
            Instruction* breakelseinst = nullptr;
            Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
            elseinst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(elseinst);
            for(Instruction *inst : cond->jump_in) {
                inst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(inst->des.name));
            }
            analyseStmt((Stmt*) root->children[4]);
            if(root->children.size() >= 6) {
                Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
                breakelseinst = new Instruction({}, {}, des, Operator::_goto);
                INSERT(breakelseinst);
            }
            elseinst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(elseinst->des.name));
            if(root->children.size() >= 6) {
                analyseStmt((Stmt*) root->children[6]);
                breakelseinst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(breakelseinst->des.name));
            }
        }
        else if(((Term*) child)->token.type == TokenType::WHILETK) {
            curr_while_stmt.push_back(root);
            Cond* cond = GET_CHILD(Cond, 2);
            curr_cond = cond;
            int condpos = current_func->InstVec.size();
            analyseCond(cond);
            curr_cond = nullptr;
            Instruction* elseinst = nullptr;
            Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
            elseinst = new Instruction({}, {}, des, Operator::_goto);
            INSERT(elseinst);
            for(Instruction *inst : cond->jump_in) {
                inst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(inst->des.name));
            }
            analyseStmt((Stmt*) root->children[4]);
            Operand des2 = Operand(std::to_string(condpos - (int) current_func->InstVec.size()), Type::IntLiteral);
            INST_INSERT({}, {}, des2, Operator::_goto, _goto);
            elseinst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(elseinst->des.name));
            for(Instruction* inst : root->jump_eow) {
                inst->des.name = std::to_string(current_func->InstVec.size() - std::stoi(inst->des.name));
            }
            for(Instruction* inst : root->jump_bow) {
                inst->des.name = std::to_string(condpos - std::stoi(inst->des.name));
            }
            curr_while_stmt.pop_back();
        }
        else if(((Term*) child)->token.type == TokenType::RETURNTK) {
            if(root->children.size() == 3){
                ANALYSIS(exp, Exp, 1);
                if(exp->t == Type::IntLiteral){ INST_INSERT(Operand(exp->v, exp->t), {}, {}, Operator::_return, _return); }
                else{ INST_INSERT(Operand(exp->v, Type::Int), {}, {}, Operator::_return, _return); }
            }
            else{ INST_INSERT({}, {}, {}, Operator::_return, _return); }
        }
    }
}
// Exp -> AddExp **
void frontend::Analyzer::analyseExp(Exp* root) {
    ANALYSIS(addexp, AddExp, 0);
    COPY_EXP_NODE(addexp, root);
}
// Cond -> LOrExp **
void frontend::Analyzer::analyseCond(Cond* root) {
    ANALYSIS(lorexp, LOrExp, 0);
    COPY_EXP_NODE(lorexp, root);
}
// LVal -> Ident {'[' Exp ']'} **
void frontend::Analyzer::analyseLVal(LVal* root) {
    Term* ident = GET_CHILD(Term, 0);
    bool is_array = root->children.size() > 1;
    STE ste = symbol_table.get_ste(ident->token.value);
    root->v = symbol_table.get_scoped_name(ident->token.value);
    root->t = Type::Int;
    if(is_array) { 
        ASSIGN_INT_TO_TMP(ZERO, des, movToDes);
        for(int i = 2; i < (int) root->children.size(); i += 3) {
            ANALYSIS(exp, Exp, i);
            ASSIGN_INT_TO_TMP(Operand(exp->v, exp->t), tmp, movToTmp);
            // des = des * dimension[i / 3] + tmp
            INST_INSERT(des, tmp, des, Operator::add, add);
            if(i + 3 < (int) root->children.size()) {
                INST_INSERT(Operand(std::to_string(ste.dimension[(i + 1) / 3]), Type::IntLiteral), {}, tmp, Operator::mov, mov2);
                INST_INSERT(des, tmp, des, Operator::mul, mul);
            }
        }
        if(root->parent->type == NodeType::PRIMARYEXP) { 
            if(ste.operand.type == Type::IntPtr) {
                Operand load_data = Operand(GET_TMP_NAME, Type::Int);
                Operand op1 = Operand(symbol_table.get_scoped_name(ident->token.value), Type::IntPtr);
                INST_INSERT(op1, des, load_data, Operator::load, load);
                root->v = load_data.name;
            }
        }
        root->i = des.name;
    }
    else {
        root->v = ste.operand.name;
        if(ste.operand.type == Type::IntLiteral) {  root->is_computable = true; root->t = Type::IntLiteral;
        } else if(ste.operand.type == Type::Int) { root->is_computable = false; root->t = Type::Int;
        } else if(ste.operand.type == Type::IntPtr) { root->is_computable = false; root->t = Type::IntPtr;
        }  
    }
}
// Number -> IntConst | floatConst **
void frontend::Analyzer::analyseNumber(Number* root) {
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
// PrimaryExp -> '(' Exp ')' | LVal | Number **
void frontend::Analyzer::analysePrimaryExp(PrimaryExp* root) {
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
// UnaryExp -> **
void frontend::Analyzer::analyseUnaryExp(UnaryExp* root) {
    auto child = root->children[0];
    auto child_type = child->type;
    // PrimaryExp
    if(child_type == NodeType::PRIMARYEXP) {
        ANALYSIS(primaryExp, PrimaryExp, 0);
        COPY_EXP_NODE(primaryExp, root);
    }
    // UnaryOp UnaryExp
    else if(child_type == NodeType::UNARYOP) {
        ANALYSIS(unaryop, UnaryOp, 0);
        ANALYSIS(unaryexp, UnaryExp, 1);
        COPY_EXP_NODE(unaryexp, root); 
        if(unaryexp->is_computable) {
            if (unaryop->op == TokenType::NOT) {
                root->v = root->v == "0" ? "1" : "0";
                root->t = Type::IntLiteral;
            }
            else if(unaryop->op == TokenType::MINU) {
                root->v = std::to_string(-std::stoi(root->v));
            }
        }
        else {
            if(unaryop->op == TokenType::NOT) {
                Operand des = Operand(root->v, root->t);
                INST_INSERT(des, ZERO, des, Operator::eq, eq);
            }
            else if(unaryop->op == TokenType::MINU) {
                ASSIGN_INT_TO_TMP(ZERO, zero, mov);
                Operand des = Operand(root->v, root->t);
                INST_INSERT(zero, des, des, Operator::sub, sub);
            }
        }
    }
    // Ident '(' [FuncRParams] ')'
    else {
        std::string func_name = GET_CHILD(Term, 0)->token.value;
        Function* func = symbol_table.functions[func_name];
        if(root->children[2]->type == NodeType::TERMINAL) {
            Operand op1 = Operand(func_name, func->returnType);
            Operand des = Operand(GET_TMP_NAME, func->returnType);
            CallInst* call = new CallInst(op1, des);
            INSERT(call);
            root->v = des.name;            
        } else {
            FuncRParams* funcRParams = GET_CHILD(FuncRParams, 2);
            std::vector<Operand> paras = analyseFuncRParams(funcRParams);
            Operand op1 = Operand(func_name, func->returnType);
            Operand des = Operand(GET_TMP_NAME, func->returnType);
            CallInst* call = new CallInst(op1, paras, des);
            INSERT(call);
            root->v = des.name;        
        }
        root->t = func->returnType;
        root->is_computable = false;
    }
}
// UnaryOp -> '+' | '-' | '!' **
void frontend::Analyzer::analyseUnaryOp(UnaryOp* root) {
    root->op = GET_CHILD(Term, 0)->token.type;
}
// FuncRParams -> Exp { ',' Exp } **
std::vector<ir::Operand> frontend::Analyzer::analyseFuncRParams(FuncRParams* root){
    std::vector<ir::Operand> paras;
    for(int i = 0; i < (int) root->children.size(); i += 2){
        ANALYSIS(exp, Exp, i);
        Operand op1 = Operand(symbol_table.get_scoped_name(exp->v), exp->t);
        if(op1.type == Type::IntLiteral) {
            ASSIGN_INT_TO_TMP(op1, des, mov);
            paras.push_back(des);
        }
        else
            paras.push_back(op1);
        // paras.push_back(op1);
    }
    return paras;
}
// MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp } **
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
                    current_func->addInst(new Instruction(op3, {}, op2, Operator::mov));
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
// AddExp -> MulExp { ('+' | '-') MulExp } **
void frontend::Analyzer::analyseAddExp(AddExp* root) {
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
                    current_func->addInst(new Instruction(op3, {}, op2, Operator::mov));
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
// RelExp -> AddExp { ('<' | '>' | '<=' | '>=') AddExp } **
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
        }
        else {
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
                Operand tmp = Operand(GET_TMP_NAME, Type::Int);
                if(addexp->t == Type::IntLiteral) {
                    if(op == Operator::leq){
                        ASSIGN_INT_TO_TMP(op3, op2, mov);
                        INST_INSERT(des, op2, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, Operator::gtr, oper);
                        INST_INSERT(des, ZERO, des, Operator::eq, eq);
                    }
                    else if(op == Operator::geq){
                        ASSIGN_INT_TO_TMP(op3, op2, mov);
                        INST_INSERT(des, op2, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, Operator::lss, oper);
                        INST_INSERT(des, ZERO, des, Operator::eq, eq);
                    }
                    else{
                        ASSIGN_INT_TO_TMP(op3, op2, mov);
                        INST_INSERT(des, op2, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, op, oper);
                    }
                } else {
                    if(op == Operator::leq){
                        INST_INSERT(des, op3, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, Operator::gtr, oper);
                        INST_INSERT(des, ZERO, des, Operator::eq, eq);
                    }
                    else if(op == Operator::geq){
                        INST_INSERT(des, op3, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, Operator::lss, oper);
                        INST_INSERT(des, ZERO, des, Operator::eq, eq);
                    }
                    else{
                        INST_INSERT(des, op3, tmp, Operator::sub, subtmp);
                        INST_INSERT(tmp, ZERO, des, op, oper);
                    }
                }
            }
            root->v = des.name;
            root->t = des.type;
        }
    }
}
// EqExp -> RelExp { ('==' | '!=') RelExp } **
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
                Operand tmp = Operand(GET_TMP_NAME, Type::Int);
                if(relexp->is_computable) {
                    ASSIGN_INT_TO_TMP(op3, op2, mov);
                    INST_INSERT(des, op2, tmp, Operator::sub, subtmp);
                    INST_INSERT(tmp, ZERO, des, op, oper);
                } else {
                    INST_INSERT(des, op3, tmp, Operator::sub, subtmp);
                    INST_INSERT(tmp, ZERO, des, op, oper);
                }
            }
            root->v = des.name;
            root->t = des.type;
        }
    }
}
// LAndExp -> EqExp [ '&&' LAndExp ] **
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
                Operand des = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
                Instruction* inst = new Instruction({}, {}, des, Operator::_goto);
                curr_cond->jump_in.insert(inst);
                INSERT(inst);
                COPY_EXP_NODE(eqexp, root);
            }
        } 
    } 
    else {
        Operand des;
        Operand op1 = Operand(eqexp->v, eqexp->t);
        Operand op2 = ZERO;
        des = Operand(eqexp->v, eqexp->t);
        INST_INSERT(op1, op2, des, Operator::eq, eq);
        int pos = current_func->InstVec.size();
        Instruction *goto_next_or = new Instruction(des, {}, ZERO, Operator::_goto);
        INSERT(goto_next_or);
        if(root->children.size() > 1) {
            ANALYSIS(landexp, LAndExp, 2);
            COPY_EXP_NODE(landexp, root);
        } else {
            Operand des2 = Operand(std::to_string(current_func->InstVec.size()), Type::IntLiteral);
            Instruction *goto_in = new Instruction({}, {}, des2, Operator::_goto);
            curr_cond->jump_in.insert(goto_in);
            INSERT(goto_in);
            COPY_EXP_NODE(eqexp, root);
        }
        goto_next_or->des.name = std::to_string(current_func->InstVec.size() - pos);
    }
}
// **
void frontend::Analyzer::analyseLOrExp(LOrExp* root) {
    ANALYSIS(landexp, LAndExp, 0)
    if(root->children.size() > 1) { ANALYSIS(lorexp, LOrExp, 2) }
    else{ COPY_EXP_NODE(landexp, root); }
}
// **
void frontend::Analyzer::analyseConstExp(ConstExp* root) {
    ANALYSIS(addexp, AddExp, 0)
    COPY_EXP_NODE(addexp, root);
}