#include"front/syntax.h"

#include<iostream>
#include<cassert>

using frontend::Parser;

// #define DEBUG_PARSER
#define TODO assert(0 && "todo")
// 判断当前token是否为tk_type
#define CUR_TOKEN_IS(tk_type) (token_stream[index].type == TokenType::tk_type)
// 对于终结符，直接将其加入到AST中
#define PARSE_TOKEN(tk_type) root->children.push_back(parseTerm(root, TokenType::tk_type))
// 对于非终结符，需要新建一个节点，然后递归调用
#define PARSE(name, type) auto name = new type(root); assert(parse##type(name)); root->children.push_back(name);
// 判断后面的token是否为tk_type
#define NEXT_TOKEN_IS(tk_type) (token_stream[index + 1].type == TokenType::tk_type)
// 判断第二个token是否为tk_type
#define NEXT_NEXT_TOKEN_IS(tk_type) (token_stream[index + 2].type == TokenType::tk_type)

Parser::Parser(const std::vector<frontend::Token>& tokens): index(0), token_stream(tokens) {}

Parser::~Parser() {}

// 结合文档的示例
frontend::CompUnit* Parser::get_abstract_syntax_tree(){
    frontend::CompUnit* root = new frontend::CompUnit(nullptr);
    parseCompUnit(root);
    return root;
}

// CompUnit -> (Decl | FuncDef) [CompUnit]
bool Parser::parseCompUnit(frontend::CompUnit* root){
    // log(root);
    // 判断是否为Decl Decl -> ConstDecl
    if(CUR_TOKEN_IS(CONSTTK)){
        PARSE(decl, Decl);
    }
    // 判断是否为Decl Decl -> VarDecl
    // 由于VarDecl和FuncDef都以int或者float开头
    // 所以需要判断后面第二位token
    // 而FuncDef的第二位token为LPARENT
    else if((CUR_TOKEN_IS(INTTK) || CUR_TOKEN_IS(FLOATTK)) && 
            !NEXT_NEXT_TOKEN_IS(LPARENT)){
        PARSE(decl, Decl);
    }
    else{
        PARSE(funcdef, FuncDef);
    }
    // [CompUnit]
    // 由于CompUnit的第一个token只能为const,int,float,VOIDTK
    if(CUR_TOKEN_IS(CONSTTK) || CUR_TOKEN_IS(VOIDTK)||CUR_TOKEN_IS(INTTK)||CUR_TOKEN_IS(FLOATTK)){
        PARSE(compunit,CompUnit);
    }
    return true;
}

// Decl -> ConstDecl | VarDecl
bool Parser::parseDecl(frontend::Decl* root){
    // log(root);
    // Decl -> ConstDecl
    if(CUR_TOKEN_IS(CONSTTK)){
        PARSE(constdecl, ConstDecl);
    }
    // Decl -> VarDecl
    else{
        PARSE(vardecl, VarDecl);
    }
    return true;
}

// ConstDecl -> 'const' BType ConstDef { ',' ConstDef } ';'
bool Parser::parseConstDecl(frontend::ConstDecl* root){
    // log(root);
    // 'const'
    PARSE_TOKEN(CONSTTK);
    // BType
    PARSE(btype, BType);
    // ConstDef
    PARSE(constdef, ConstDef);
    // { ',' ConstDef }，循环判断后一个Token是不是','，如果是则继续解析ConstDef
    while(CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);
        PARSE(constdef, ConstDef);
    }
    // ';'
    PARSE_TOKEN(SEMICN);
    return true;
}

// BType -> 'int' | 'float'
bool Parser::parseBType(frontend::BType* root){
    // log(root);
    if(CUR_TOKEN_IS(INTTK)) PARSE_TOKEN(INTTK);
    else PARSE_TOKEN(FLOATTK);
    return true;
}

// ConstDef -> Ident { '[' ConstExp ']' } '=' ConstInitVal
bool Parser::parseConstDef(frontend::ConstDef* root){
    // log(root);
    // Ident
    PARSE_TOKEN(IDENFR);
    // { '[' ConstExp ']' }
    while(CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE(constexp, ConstExp);
        PARSE_TOKEN(RBRACK);
    }
    // '='
    PARSE_TOKEN(ASSIGN);
    // ConstInitVal
    PARSE(constinitval, ConstInitVal);
    return true;
}

// ConstInitVal -> ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
bool Parser::parseConstInitVal(frontend::ConstInitVal* root){
    // log(root);
    // ConstExp的first集元素过多，且不包含{，所以直接判断是否为{
    // '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
    if(CUR_TOKEN_IS(LBRACE)){
        PARSE_TOKEN(LBRACE);
        // 如果不是右大括号，那么就是ConstInitVal
        if(!CUR_TOKEN_IS(RBRACE)){
            // [ ConstInitVal { ',' ConstInitVal } ]
            PARSE(constinitval, ConstInitVal);
            while(CUR_TOKEN_IS(COMMA)){
                PARSE_TOKEN(COMMA);
                PARSE(constinitval, ConstInitVal);
            }
        }
        // '}'
        PARSE_TOKEN(RBRACE);
    }
    else{
        PARSE(constexp, ConstExp);
    }
    return true;
}

// VarDecl -> BType VarDef { ',' VarDef } ';'
bool Parser::parseVarDecl(frontend::VarDecl* root){
    // log(root);
    // BType
    PARSE(btype, BType);
    // VarDef
    PARSE(vardef, VarDef);
    // { ',' VarDef }
    while(CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);
        PARSE(vardef, VarDef);
    }
    // ';'
    PARSE_TOKEN(SEMICN);
    return true;
}

// VarDef -> Ident { '[' ConstExp ']' } [ '=' InitVal ]
bool Parser::parseVarDef(frontend::VarDef* root){
    // log(root);
    // Ident
    PARSE_TOKEN(IDENFR);
    // { '[' ConstExp ']' }
    while(CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE(constexp, ConstExp);
        PARSE_TOKEN(RBRACK);
    }
    // [ '=' InitVal ]
    if(CUR_TOKEN_IS(ASSIGN)){
        PARSE_TOKEN(ASSIGN);
        PARSE(initval, InitVal);
    }
    return true;
}

// InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}'
bool Parser::parseInitVal(frontend::InitVal* root){
    // log(root);
    // Exp的first集元素过多，且不包含{，所以直接判断是否为{
    // '{' [ InitVal { ',' InitVal } ] '}'
    if(CUR_TOKEN_IS(LBRACE)){
        PARSE_TOKEN(LBRACE);
        // 如果不是右大括号，那么就是InitVal
        if(!CUR_TOKEN_IS(RBRACE)){
            // [ InitVal { ',' InitVal } ]
            PARSE(initval, InitVal);
            while(CUR_TOKEN_IS(COMMA)){
                PARSE_TOKEN(COMMA);
                PARSE(initval, InitVal);
            }
        }
        // '}'
        PARSE_TOKEN(RBRACE);
    }
    else{
        PARSE(exp, Exp);
    }
    return true;
}

// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
bool Parser::parseFuncDef(frontend::FuncDef* root){
    // log(root);
    // FuncType
    PARSE(functype, FuncType);
    // Ident
    PARSE_TOKEN(IDENFR);
    // '('
    PARSE_TOKEN(LPARENT);
    // [FuncFParams]
    if(!CUR_TOKEN_IS(RPARENT)){
        PARSE(funcfparams, FuncFParams);
    }
    // ')'
    PARSE_TOKEN(RPARENT);
    // Block
    PARSE(block, Block);
    return true;
}

// FuncType -> 'void' | 'int' | 'float'
bool Parser::parseFuncType(frontend::FuncType* root){
    // log(root);
    if(CUR_TOKEN_IS(VOIDTK)) PARSE_TOKEN(VOIDTK);
    else if(CUR_TOKEN_IS(INTTK)) PARSE_TOKEN(INTTK);
    else PARSE_TOKEN(FLOATTK);
    return true;
}

// FuncFParam -> BType Ident ['[' ']' { '[' Exp ']' }]
bool Parser::parseFuncFParam(frontend::FuncFParam* root){
    // log(root);
    // BType
    PARSE(btype, BType);
    // Ident
    PARSE_TOKEN(IDENFR);
    // ['[' ']' { '[' Exp ']' }]
    if(CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE_TOKEN(RBRACK);
        // { '[' Exp ']' }
        while(CUR_TOKEN_IS(LBRACK)){
            PARSE_TOKEN(LBRACK);
            PARSE(exp, Exp);
            PARSE_TOKEN(RBRACK);
        }
    }
    return true;
}

// FuncFParams -> FuncFParam { ',' FuncFParam }
bool Parser::parseFuncFParams(frontend::FuncFParams* root){
    // log(root);
    // FuncFParam
    PARSE(funcfparam, FuncFParam);
    // { ',' FuncFParam }
    while(CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);
        PARSE(funcfparam, FuncFParam);
    }
    return true;
}

// Block -> '{' { BlockItem } '}'
bool Parser::parseBlock(frontend::Block* root){
    // log(root);
    // '{'
    PARSE_TOKEN(LBRACE);
    // { BlockItem }
    while (!CUR_TOKEN_IS(RBRACE)){
        PARSE(blockitem, BlockItem);
    }
    // '}'
    PARSE_TOKEN(RBRACE);
    return true;
}

// BlockItem -> Decl | Stmt
bool Parser::parseBlockItem(frontend::BlockItem* root){
    // log(root);
    // Decl
    if(CUR_TOKEN_IS(CONSTTK) || CUR_TOKEN_IS(INTTK) || CUR_TOKEN_IS(FLOATTK)){
        PARSE(decl, Decl);
    }
    // Stmt
    else{
        PARSE(stmt, Stmt);
    }
    return true;
}

// * Stmt -> LVal '=' Exp ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ] | 'while' '(' Cond ')' Stmt | 'break' ';' | 'continue' ';' | 'return' [Exp] ';' | [Exp] ';'
bool Parser::parseStmt(frontend::Stmt* root){
    // log(root);
    // LVal '=' Exp ';'
    if(CUR_TOKEN_IS(IDENFR) && !NEXT_TOKEN_IS(LPARENT)){
        PARSE(lval, LVal);
        PARSE_TOKEN(ASSIGN);
        PARSE(exp, Exp);
        PARSE_TOKEN(SEMICN);
    }
    // Block
    else if(CUR_TOKEN_IS(LBRACE)){
        PARSE(block, Block);
    }
    // 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
    else if(CUR_TOKEN_IS(IFTK)){
        PARSE_TOKEN(IFTK);
        PARSE_TOKEN(LPARENT);
        PARSE(cond, Cond);
        PARSE_TOKEN(RPARENT);
        PARSE(stmt, Stmt);
        if(CUR_TOKEN_IS(ELSETK)){
            PARSE_TOKEN(ELSETK);
            PARSE(stmt, Stmt);
        }
    }
    // 'while' '(' Cond ')' Stmt
    else if(CUR_TOKEN_IS(WHILETK)){
        PARSE_TOKEN(WHILETK);
        PARSE_TOKEN(LPARENT);
        PARSE(cond, Cond);
        PARSE_TOKEN(RPARENT);
        PARSE(stmt, Stmt);
    }
    // 'break' ';'
    else if(CUR_TOKEN_IS(BREAKTK)){
        PARSE_TOKEN(BREAKTK);
        PARSE_TOKEN(SEMICN);
    }
    // 'continue' ';'
    else if(CUR_TOKEN_IS(CONTINUETK)){
        PARSE_TOKEN(CONTINUETK);
        PARSE_TOKEN(SEMICN);
    }
    // 'return' [Exp] ';'
    else if(CUR_TOKEN_IS(RETURNTK)){
        PARSE_TOKEN(RETURNTK);
        if(!CUR_TOKEN_IS(SEMICN)){
            PARSE(exp, Exp);
        }
        PARSE_TOKEN(SEMICN);
    }
    // [Exp] ';'
    else{
        if(!CUR_TOKEN_IS(SEMICN)){
            PARSE(exp, Exp);
        }
        PARSE_TOKEN(SEMICN);
    }
    return true;
}

// Exp -> AddExp
bool Parser::parseExp(frontend::Exp* root){
    // log(root);
    PARSE(addexp, AddExp);
    return true;
}

// Cond -> LOrExp
bool Parser::parseCond(frontend::Cond* root){
    // log(root);
    PARSE(lorexp, LOrExp);
    return true;
}

// LVal -> Ident {'[' Exp ']'}
bool Parser::parseLVal(frontend::LVal* root){
    // log(root);
    PARSE_TOKEN(IDENFR);
    while(CUR_TOKEN_IS(LBRACK)){
        PARSE_TOKEN(LBRACK);
        PARSE(exp, Exp);
        PARSE_TOKEN(RBRACK);
    }
    return true;
}

// Number -> IntConst | floatConst
bool Parser::parseNumber(frontend::Number* root){
    // log(root);
    if(CUR_TOKEN_IS(INTLTR)){
        PARSE_TOKEN(INTLTR);
    }
    else{
        PARSE_TOKEN(FLOATLTR);
    }
    return true;
}

// PrimaryExp -> '(' Exp ')' | LVal | Number *
bool Parser::parsePrimaryExp(frontend::PrimaryExp* root){
    log(root);
    if(CUR_TOKEN_IS(LPARENT)){
        PARSE_TOKEN(LPARENT);
        PARSE(exp,Exp);
        PARSE_TOKEN(RPARENT);
    }
    else if(CUR_TOKEN_IS(IDENFR)){
        PARSE(lval,LVal);
    }
    else{
        PARSE(number,Number);
    }
    return true;
}

// UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp *
bool Parser::parseUnaryExp(UnaryExp* root){
    // log(root);
    // Ident '(' [FuncRParams] ')'
    if(CUR_TOKEN_IS(IDENFR) && NEXT_TOKEN_IS(LPARENT)){
        PARSE_TOKEN(IDENFR);
        PARSE_TOKEN(LPARENT);
        // Ident '(' [FuncRParams] ')'
        if(!CUR_TOKEN_IS(RPARENT)){
            PARSE(funcrparams, FuncRParams);
        }
        PARSE_TOKEN(RPARENT);
    }
    // UnaryOp UnaryExp
    else if(CUR_TOKEN_IS(PLUS) || CUR_TOKEN_IS(MINU) || CUR_TOKEN_IS(NOT)){
        PARSE(unaryop,UnaryOp);
        PARSE(unaryexp,UnaryExp);
    }
    // PrimaryExp
    else{
        PARSE(primaryexp,PrimaryExp);
    }
    return true;
}

// UnaryOp -> '+' | '-' | '!'
bool Parser::parseUnaryOp(frontend::UnaryOp* root){
    // log(root);
    if(CUR_TOKEN_IS(PLUS)) PARSE_TOKEN(PLUS);
    else if(CUR_TOKEN_IS(MINU)) PARSE_TOKEN(MINU);
    else PARSE_TOKEN(NOT);
    return true;
}

// FuncRParams -> Exp { ',' Exp }
bool Parser::parseFuncRParams(frontend::FuncRParams* root){
    // log(root);
    PARSE(exp, Exp);
    while(CUR_TOKEN_IS(COMMA)){
        PARSE_TOKEN(COMMA);
        PARSE(exp, Exp);
    }
    return true;
}

// MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
bool Parser::parseMulExp(frontend::MulExp* root){
    // log(root);
    PARSE(unaryexp, UnaryExp);
    while(CUR_TOKEN_IS(MULT) || CUR_TOKEN_IS(DIV) || CUR_TOKEN_IS(MOD)){
        if(CUR_TOKEN_IS(MULT)) PARSE_TOKEN(MULT);
        else if(CUR_TOKEN_IS(DIV)) PARSE_TOKEN(DIV);
        else PARSE_TOKEN(MOD);
        PARSE(unaryexp, UnaryExp);
    }
    return true;
}

// AddExp -> MulExp { ('+' | '-') MulExp }
bool Parser::parseAddExp(frontend::AddExp* root){
    // log(root);
    PARSE(mulexp, MulExp);
    while(CUR_TOKEN_IS(PLUS) || CUR_TOKEN_IS(MINU)){
        if(CUR_TOKEN_IS(PLUS)) PARSE_TOKEN(PLUS);
        else PARSE_TOKEN(MINU);
        PARSE(mulexp, MulExp);
    }
    return true;
}

// RelExp -> AddExp { ('<' | '>' | '<=' | '>=') AddExp }
bool Parser::parseRelExp(frontend::RelExp* root){
    // log(root);
    PARSE(addexp, AddExp);
    while(CUR_TOKEN_IS(LSS) || CUR_TOKEN_IS(LEQ) || CUR_TOKEN_IS(GTR) || CUR_TOKEN_IS(GEQ)){
        if(CUR_TOKEN_IS(LSS)) PARSE_TOKEN(LSS);
        else if(CUR_TOKEN_IS(LEQ)) PARSE_TOKEN(LEQ);
        else if(CUR_TOKEN_IS(GEQ)) PARSE_TOKEN(GEQ);
        else PARSE_TOKEN(GTR);
        PARSE(addexp, AddExp);
    }
    return true;
}

// EqExp -> RelExp { ('==' | '!=') RelExp }
bool Parser::parseEqExp(frontend::EqExp* root){
    // log(root);
    PARSE(relexp, RelExp);
    while(CUR_TOKEN_IS(EQL) || CUR_TOKEN_IS(NEQ)){
        if(CUR_TOKEN_IS(EQL)) PARSE_TOKEN(EQL);
        else PARSE_TOKEN(NEQ);
        PARSE(relexp, RelExp);
    }
    return true;
}

// LAndExp -> EqExp [ '&&' LAndExp ]
bool Parser::parseLAndExp(frontend::LAndExp* root){
    // log(root);
    PARSE(eqexp, EqExp);
    if(CUR_TOKEN_IS(AND)){
        PARSE_TOKEN(AND);
        PARSE(landexp, LAndExp);
    }
    return true;
}

// LOrExp -> LAndExp [ '||' LOrExp ]
bool Parser::parseLOrExp(frontend::LOrExp* root){
    // log(root);
    PARSE(landexp, LAndExp);
    if(CUR_TOKEN_IS(OR)){
        PARSE_TOKEN(OR);
        PARSE(lorexp, LOrExp);
    }
    return true;
}

// ConstExp -> AddExp
bool Parser::parseConstExp(frontend::ConstExp* root){
    // log(root);
    PARSE(addexp, AddExp);
    return true;
}

// Term
frontend::Term* Parser::parseTerm(AstNode* parent, TokenType expected){
    frontend::Term* term = new frontend::Term(token_stream[index],parent);
    index++;
    return term;
}


void Parser::log(AstNode* node){
#ifdef DEBUG_PARSER
        std::cout << "in parse" << toString(node->type) << ", cur_token_type::" << toString(token_stream[index].type) << ", token_val::" << token_stream[index].value << '\n';
#endif
}
