#include"front/semantic.h"
#include<exception>
#include<cassert>

using ir::Instruction;
using ir::Function;
using ir::Operand;
using ir::Operator;

#define TODO assert(0 && "TODO");
// get_child_ptr 用于获取根节点的第 index 个孩子节点，并且将其转换为 type 类型的指针，assert 用于确保转换成功
#define GET_CHILD_PTR(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node); 
// get_child_ptr_no 不带声明的get_child_ptr
#define GET_CHILD_PTR_NO(type, index) dynamic_cast<type*>(root->children[index]);
// analysis 用于对根节点的第 index 个孩子节点进行分析
#define ANALYSIS(node, type, index) auto node = dynamic_cast<type*>(root->children[index]); assert(node); frontend::Analyzer::analysis##type(node, buffer);
// copy_exp_node 用于将 from 节点的信息复制到 to 节点
#define COPY_EXP_NODE(from, to) to->is_computable = from->is_computable; to->v = from->v; to->t = from->t;
// cvt_nd_opd 用于将 node 节点转换为操作数
#define CVT_ND_OPD(node) Operand(node->v, node->t)
// tttt 用于将tokenType转换为Type
#define TTTT(tokenType) (tokenType == frontend::TokenType::INTTK ? Type::Int : (tokenType == TokenType::FLOATTK ? Type::Float : Type::null))
// isFloat 用于判断Type是否为Float
#define IS_FLOAT(type) (type == Type::Float || type == Type::FloatPtr || type == Type::FloatLiteral)
// typeCase 用于将Operand的Type改为对应的type，并添加指令
#define TYPE_CAST_ITF(operand, destype) buffer.push_back(new Instruction(operand, {}, Operand(operand->name, destype), Operator::cvt_i2f)); operand->type = destype;
#define TYPE_CAST_FTI(operand, destype) buffer.push_back(new Instruction(operand, {}, Operand(operand->name, destype), Operator::cvt_f2i)); operand->type = destype;


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
    string scope_name = "Scope" + std::to_string(scopeID);
    // 初始化符号表
    map_str_ste ste = map_str_ste();
    // 将Block节点的信息加入符号表
    ScopeInfo scope_info = {scopeID, scope_name, ste};
    // 将该Scope的信息加入符号表
    scope_stack.push_back(scope_info);
}

// 退出Scope
void frontend::SymbolTable::exit_scope() {
    scope_stack.pop_back();
}

string frontend::SymbolTable::get_scoped_name(string id) const {
    // 栈顶的Scope的编号
    int scopeID = scope_stack.back().cnt;
    // 返回重命名后的id
    return id + "_" + std::to_string(scopeID);
}

Operand frontend::SymbolTable::get_operand(string id) const {
    // 先检查栈顶的Scope中是否有该id
    ScopeInfo top_scope = scope_stack.back();
    map_str_ste top_map = top_scope.table;
    if (top_map.find(id) != top_map.end()) {
        // 如果有，返回该id对应的操作数
        return top_map.at(id).operand; // 这个变量在加入符号表时就应该被重命名
    }
    // 如果没有，检查栈底Scope中是否有该id
    else{
        ScopeInfo bottom_scope = scope_stack.front();
        map_str_ste bottom_map = bottom_scope.table;
        if (bottom_map.find(id) != bottom_map.end()) {
            // 如果有，返回该id对应的操作数
            return bottom_map.at(id).operand; // 这个变量在加入符号表时就应该被重命名
        }
        // 如果没有，返回空操作数
        else{
            return Operand();
        }
    }
}

// 实现与get_operand类似的功能，但是不返回操作数，而是返回STE
frontend::STE frontend::SymbolTable::get_ste(string id) const {
    // 先检查栈顶的Scope中是否有该id
    ScopeInfo top_scope = scope_stack.back();
    map_str_ste top_map = top_scope.table;
    if (top_map.find(id) != top_map.end()) {
        // 如果有，返回该id对应的STE
        return top_map.at(id);
    }
    // 如果没有，检查栈底Scope中是否有该id
    else{
        ScopeInfo bottom_scope = scope_stack.front();
        map_str_ste bottom_map = bottom_scope.table;
        if (bottom_map.find(id) != bottom_map.end()) {
            // 如果有，返回该id对应的STE
            return bottom_map.at(id);
        }
        // 如果没有，返回空STE
        else{
            return STE();
        }
    }
}

// Analysis构造函数
frontend::Analyzer::Analyzer(): tmp_cnt(0), symbol_table() {
    // 初始化符号表
    symbol_table.cnt = 0;
    symbol_table.scope_stack = vector<frontend::ScopeInfo>();
    // 添加静态函数
    symbol_table.functions = *get_lib_funcs();
}

// 本次实验的接口
ir::Program frontend::Analyzer::get_ir_program(CompUnit* root) {
    // 用于存储IR指令的缓冲区
    vector<ir::Instruction*> buffer;
    // 全局函数声明
    std::vector<ir::Function> functions;
    // 全局变量声明
    std::vector<ir::GlobalVal> globalVal;
    // 程序体声明
    ir::Program program = ir::Program();
    program.functions = functions;
    program.globalVal = globalVal;
    // 分析CompUnit节点
    analysisCompUnit(root, program);
    // 返回IR程序
    return program;
}

// CompUnit -> (Decl | FuncDef) [CompUnit]
// Decl -> ConstDecl | VarDecl
// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
// 1. int main() {int a;}
void frontend::Analyzer::analysisCompUnit(CompUnit* root, ir::Program& program){
    
}

// PPT 示例1
// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
// FuncType -> void | int | float
// FuncDef.t 
// FuncDef.n 
void frontend::Analyzer::analysisFuncDef(FuncDef* root, ir::Function& function){
    // 获取返回类型
    auto tk = dynamic_cast<Term*> (root->children[0]->children[0])->token.type;
    // 初始化t和n属性
    // TokenTypeToType
    root->t = TTTT(tk);
    // 获取函数名
    root->n = dynamic_cast<Term*> (root->children[1])->token.value;
    // 初始化函数属性
    function.name = root->n;
    function.returnType = root->t;
    // 当一个函数建立后，会进入到该block的符号表中
    //******************这里做出了改动******************
    symbol_table.scope_stack.push_back({symbol_table.cnt++, "fp", map_str_ste()});
    symbol_table.functions.insert({root->n, &function});
    // 解析参数
    auto paras = dynamic_cast<FuncFParams*>(root->children[3]);
    // 如果是有参构造
    if(paras){
        auto block = dynamic_cast<Block*>(root->children[5]);
        analysisFuncFParams(paras, function);
        analysisBlock(block, function.InstVec);
    }
    else{
        auto block = dynamic_cast<Block*>(root->children[4]);
        analysisBlock(block, function.InstVec);
    }
    // 函数结束后退出当前作用域
    symbol_table.exit_scope();
}
void frontend::Analyzer::analysisDecl(Decl* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisConstDecl(ConstDecl* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisBType(BType* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisConstDef(ConstDef* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisConstInitVal(ConstInitVal* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisVarDecl(VarDecl* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisVarDef(VarDef* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisInitVal(InitVal* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisFuncType(FuncType* root, vector<ir::Instruction*>& buffer){}
/* Function */
void frontend::Analyzer::analysisFuncFParam(FuncFParam* root, ir::Function& function){}
/* Function */
void frontend::Analyzer::analysisFuncFParams(FuncFParams* root, ir::Function& function){}
void frontend::Analyzer::analysisBlock(Block* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisBlockItem(BlockItem* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisStmt(Stmt* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisExp(Exp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisCond(Cond* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisLVal(LVal* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisNumber(Number* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisPrimaryExp(PrimaryExp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisUnaryExp(UnaryExp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisUnaryOp(UnaryOp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisFuncRParams(FuncRParams* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisMulExp(MulExp* root, vector<ir::Instruction*>& buffer){}

// AddExp -> MulExp { ('+' | '-') MulExp }
// MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
// AddExp.is_computable
// AddExp.v
// AddExp.t
void frontend::Analyzer::analysisAddExp(AddExp* root, vector<ir::Instruction*>& buffer){
    // AddExp -> MulExp
    if(root->children.size() == 1){
        ANALYSIS(node, MulExp, 0);
        // 向上传递
        COPY_EXP_NODE(node, root);
        // IR指令
        buffer.push_back(new Instruction({CVT_ND_OPD(node), {}, CVT_ND_OPD(root), Operator::mov}));
    }
    else{
        ANALYSIS(mulexp1, MulExp, 0);
        COPY_EXP_NODE(mulexp1, root);
        // 产生临时变量
        root->v = "t" + std::to_string(symbol_table.cnt++);
        // 之后直接在该临时变量中操作
        buffer.push_back(new Instruction({CVT_ND_OPD(mulexp1), {}, CVT_ND_OPD(root), Operator::mov}));
        // 由于不确定最终的类型，需要提前判断，只要有一个是float，那么结果就是float
        for(int i = 2; i < root->children.size(); i += 2){
            ANALYSIS(mulexp, MulExp, i);
            // 如果类型不一致，进行类型转换
            if(mulexp->t == Type::Float && root->t == Type::Int){
                // 需要将root进行类型转换
                Operand opd_cv = Operand(CVT_ND_OPD(root).name + "_f", Type::Float);
                buffer.push_back(new Instruction(CVT_ND_OPD(root), {}, opd_cv, Operator::cvt_i2f));
                root->t = opd_cv.type;
                root->v = opd_cv.name;
            }
            else if(mulexp->t == Type::Int && root->t == Type::Float){
                // 需要将mulexp进行类型转换
                Operand opd_cv = Operand(CVT_ND_OPD(mulexp).name + "_f", Type::Float);
                buffer.push_back(new Instruction(CVT_ND_OPD(mulexp), {}, opd_cv, Operator::cvt_i2f));
                mulexp->t = opd_cv.type;
                mulexp->v = opd_cv.name;
            }
            else{
                // 两者类型相同，不需要转换
            }
            // 之后进行加法操作
            if(dynamic_cast<Term*>(root->children[i - 1])->token.type == TokenType::PLUS){
                buffer.push_back(new Instruction({CVT_ND_OPD(mulexp), CVT_ND_OPD(root), CVT_ND_OPD(root), Operator::add}));
            }
            else{
                buffer.push_back(new Instruction({CVT_ND_OPD(mulexp), CVT_ND_OPD(root), CVT_ND_OPD(root), Operator::sub}));
            }
        }
    }

}
void frontend::Analyzer::analysisRelExp(RelExp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisEqExp(EqExp* root, vector<ir::Instruction*>& buffer){}
void frontend::Analyzer::analysisLAndExp(LAndExp* root, vector<ir::Instruction*>& buffer){}

// PPT 示例二
// LOrExp -> LAndExp [ '||' LOrExp ]
// LAndExp -> EqExp [ '&&' LAndExp ]
// LOrExp.is_computable 
// LOrExp.v 
// LOrExp.t
void frontend::Analyzer::analysisLOrExp(LOrExp* root, vector<ir::Instruction*>& buffer){
    // 如果 LOrExp -> LAndExp
    if(root->children.size() == 1){
        ANALYSIS(node, LAndExp, 0);
        // 向上传递
        COPY_EXP_NODE(node, root);
        // IR指令添加
        buffer.push_back(new Instruction(CVT_ND_OPD(node), {}, CVT_ND_OPD(root), Operator::mov));
    }
    else{
        ANALYSIS(andexp, LAndExp, 0);
        // ***注意产生临时变量的处理***
        auto tmp = vector<Instruction*>();
        GET_CHILD_PTR(orexp, LOrExp, 2);
        // 注意产生的其他IR指令放到了tmp中
        // 使用tmp的原因是要去支持短路运算
        analysisLOrExp(orexp, tmp);
        // 由于这是一个逻辑运算，所以需要产生一个临时变量
        root->v = "t" + std::to_string(tmp_cnt++);
        root->t = Type::Int;
        Operand op1 = CVT_ND_OPD(andexp);
        Operand op2 = CVT_ND_OPD(orexp);
        Operand des = CVT_ND_OPD(root);
        // 支持短路运算
        // if op1, goto pc + 3 + tmp.size()
        Instruction* ins = new Instruction(op1, {}, {std::to_string(tmp.size() + 3), Type::IntLiteral}, Operator::_goto);
        buffer.push_back(ins);
        // 将tmp产生的指令放到buffer中
        buffer.insert(buffer.end(), tmp.begin(), tmp.end());
        // 既然能执行到这条命令，说明op1 = 0，或运算的结果取决于op2
        buffer.push_back(new Instruction({op2, {}, des, Operator::mov}));
        // goto pc + 2
        buffer.push_back(new Instruction({}, {}, {"2", Type::IntLiteral}, Operator::_goto));
        // des = 1 如果op1 = 1，那么直接跳过op2的计算，直接将des置为1
        buffer.push_back(new Instruction({"1", Type::IntLiteral}, {}, des, Operator::mov));
    }
}
void frontend::Analyzer::analysisConstExp(ConstExp* root, vector<ir::Instruction*>& buffer){}