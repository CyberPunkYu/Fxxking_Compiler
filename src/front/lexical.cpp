#include"front/lexical.h"

#include<map>
#include<cassert>
#include<string>
#define DEBUG_DFA
#define TODO assert(0 && "todo")

// #define DEBUG_DFA
// #define DEBUG_SCANNER

// 有限状态机的五个状态String化
std::string frontend::toString(State s) {
    switch (s) {
    case State::Empty: return "Empty";
    case State::Ident: return "Ident";
    case State::IntLiteral: return "IntLiteral";
    case State::FloatLiteral: return "FloatLiteral";
    case State::op: return "op";
    default:
        assert(0 && "invalid State");
    }
    return "";
}

// 关键字表
std::set<std::string> frontend::keywords = {
    "const", "int", "float", "if", "else", "while", "continue", "break", "return", "void"
};
// 运算符表，运算符见token.h
std::set<std::string> frontend::operaters = {
    "+", "-", "*", "/", "%", "<", ">", ":", "=", ";", ",", "(", ")", "[", "]", "{", "}","<=", ">=", "==", "!=", "&&", "||", "!", "|", "&"
};
// 将识别到的关键字转换为Token，返回类型为TokenType
frontend::TokenType frontend::get_key_TokenType(std::string key){
    static std::map<std::string, frontend::TokenType> key2tk = {
        {"const", frontend::TokenType::CONSTTK},
        {"int", frontend::TokenType::INTTK},
        {"float", frontend::TokenType::FLOATTK},
        {"if", frontend::TokenType::IFTK},
        {"else", frontend::TokenType::ELSETK},
        {"while", frontend::TokenType::WHILETK},
        {"continue", frontend::TokenType::CONTINUETK},
        {"break", frontend::TokenType::BREAKTK},
        {"return", frontend::TokenType::RETURNTK},
        {"void", frontend::TokenType::VOIDTK}
    };
    // 如果这个key不在关键字表中，返回IDENFR
    if(key2tk.find(key) == key2tk.end())
        return frontend::TokenType::IDENFR;
    return key2tk[key];
}
// 将识别到的运算符转换为Token，返回类型为TokenType
frontend::TokenType frontend::get_op_TokenType(std::string o){
    static std::map<std::string, frontend::TokenType> op2tk = {
        {"+", frontend::TokenType::PLUS},
        {"-", frontend::TokenType::MINU},
        {"*", frontend::TokenType::MULT},
        {"/", frontend::TokenType::DIV},
        {"%", frontend::TokenType::MOD},
        {"<", frontend::TokenType::LSS},
        {">", frontend::TokenType::GTR},
        {":", frontend::TokenType::COLON},
        {"=", frontend::TokenType::ASSIGN},
        {";", frontend::TokenType::SEMICN},
        {",", frontend::TokenType::COMMA},
        {"(", frontend::TokenType::LPARENT},
        {")", frontend::TokenType::RPARENT},
        {"[", frontend::TokenType::LBRACK},
        {"]", frontend::TokenType::RBRACK},
        {"{", frontend::TokenType::LBRACE},
        {"}", frontend::TokenType::RBRACE},
        {"<=", frontend::TokenType::LEQ},
        {">=", frontend::TokenType::GEQ},
        {"==", frontend::TokenType::EQL},
        {"!=", frontend::TokenType::NEQ},
        {"&&", frontend::TokenType::AND},
        {"||", frontend::TokenType::OR},
        {"!", frontend::TokenType::NOT}
    };
    // 如果这个key不在运算符表中，返回IDENFR
    if(op2tk.find(o) == op2tk.end())
        return frontend::TokenType::IDENFR;
    return op2tk[o];
}


// 判断是否为数字
bool frontend::isNumber(char c){
    if(c >= '0' && c <= '9')
        return true;
    return false;
}
// 判断是否为字母
bool frontend::isLetter(char c){
    // 注意：这里的下划线也算是字母
    if((c <= 'z' && c >= 'a') || (c <= 'Z' && c >= 'A') || (c == '_'))
        return true;
    return false;
}

// 构造函数
frontend::DFA::DFA(): cur_state(frontend::State::Empty), cur_str() {}

// 析构函数
frontend::DFA::~DFA() {}

// 有限状态机的状态转移函数
bool frontend::DFA::next(char input, Token& buf) {
#ifdef DEBUG_DFA
#include<iostream>
    std::cout << "in state [" << toString(cur_state) << "], input = \'" << input << "\', str = " << cur_str << std::endl;
#endif
    // Empty, Ident, IntLiteral, FloatLiteral, op 
    // 判断读入字符对应的状态
    State char_state;
    if(isLetter(input))
        // 如果是字母，那么就是标识符
        char_state = State::Ident;
    else if(isNumber(input))
        // 如果是数字，那么就是整数或者浮点数
        char_state = State::IntLiteral;
    else if(input == '.')
        // 如果是小数点，那么就是浮点数
        char_state = State::FloatLiteral;
    else if(operaters.find(std::string(1, input)) != operaters.end())
        // 如果是运算符，那么就是运算符
        char_state = State::op;
    else
        // 如果是其他字符，那么就是空
        char_state = State::Empty;
    // 状态转移
    bool ret = false;
    // 如果当前状态是Empty
    if(cur_state == frontend::State::Empty){
        // 如果不是空格，tab和换行，直接转移到读入状态
        if(char_state != frontend::State::Empty){
            cur_state = char_state;
            cur_str += input;
        }
        // 如果是空格，tab和换行，那么就是空
        else{
            reset();
        }
    }
    // 如果当前状态是Ident
    else if(cur_state == frontend::State::Ident){
        // 对于该标识符，如果读入字符是字母或者数字或者下划线，那么继续保持Ident状态
        if(char_state == frontend::State::Ident || char_state == frontend::State::IntLiteral){
            cur_state = frontend::State::Ident;
            cur_str += input;
        }
        else if (char_state == frontend::State::op){
            // 如果读入字符是运算符，那么就是标识符
            cur_state = frontend::State::op;
            // 那么之前的字符串就可以输出了
            buf.type = frontend::get_key_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            cur_str = input;
        }
        else{
            // 如果读入字符是其他字符，那么就是标识符
            buf.type = frontend::get_key_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            reset();
        }
    }
    // 如果当前状态是IntLiteral
    else if (cur_state == frontend::State::IntLiteral){
        // 对于该整数，如果读入字符是数字，那么继续保持IntLiteral状态
        // 新增进制判断，由于二进制和十六进制可能会出现字母，所以需要特殊处理
        // 判断是否为十六进制
        bool flag = false;
        // 如果当前字符串包含了x或者X，那么就是十六进制
        if(cur_str.find('x') != std::string::npos || cur_str.find('X') != std::string::npos)
            flag = true;
        // 需要额外判断是不是十六进制会出现的合法字符
        if(char_state == frontend::State::IntLiteral || 
        (cur_str == "0" && (input == 'x' || input == 'X' || input == 'b' || input == 'B')) ||
        (flag && ((input >= 'a' && input <= 'f') || (input >= 'A' && input <= 'F')))){
            cur_state = frontend::State::IntLiteral;
            cur_str += input;
        }
        else if(char_state == frontend::State::FloatLiteral){
            // 如果读入字符是小数点，那么就是浮点数
            cur_state = frontend::State::FloatLiteral;
            cur_str += input;
        }
        else if(char_state == frontend::State::op){
            // 如果读入字符是运算符，那么就是整数
            cur_state = frontend::State::op;
            // 那么之前的字符串就可以输出了
            buf.type = frontend::TokenType::INTLTR;
            buf.value = cur_str;
            ret = true;
            cur_str = input;
        }
        else{
            // 如果读入字符是其他字符，那么就是整数
            buf.type = frontend::TokenType::INTLTR;
            buf.value = cur_str;
            ret = true;
            reset();
        }
    }
    // 如果当前状态是FloatLiteral
    else if(cur_state == frontend::State::FloatLiteral){
        // 对于该浮点数，如果读入字符是数字，那么继续保持FloatLiteral状态
        if(char_state == frontend::State::IntLiteral){
            cur_state = frontend::State::FloatLiteral;
            cur_str += input;
        }
        else if(char_state == frontend::State::op){
            // 如果读入字符是运算符，那么就是浮点数
            cur_state = frontend::State::op;
            // 那么之前的字符串就可以输出了
            buf.type = frontend::TokenType::FLOATLTR;
            buf.value = cur_str;
            ret = true;
            cur_str = input;
        }
        else{
            // 如果读入字符是其他字符，那么就是浮点数
            buf.type = frontend::TokenType::FLOATLTR;
            buf.value = cur_str;
            ret = true;
            reset();
        }
    }
    // 如果当前状态是op
    else if(cur_state == frontend::State::op){
        // 根据状态转移图，对于部分符号，可能出现多个符号连在一起的情况，需要单独判断
        if(char_state == frontend::State::op){
            // char和cur都是运算符，先判断是否是双运算符
            std::string tmp = cur_str + input;
            if(operaters.find(tmp) != operaters.end()){
                // 如果是双运算符，那么就是运算符，下次读入的时候输出
                cur_state = frontend::State::op;
                cur_str = tmp;
            }
            else{
                // 如果不是双运算符，那么就是单运算符，直接输出
                buf.type = frontend::get_op_TokenType(cur_str);
                buf.value = cur_str;
                ret = true;
                cur_state = frontend::State::op;
                cur_str   = input;
            }
        }
        // 如果读入字符是数字，那么输出之前的运算符，然后转移到IntLiteral状态
        else if(char_state == frontend::State::IntLiteral){
            buf.type = frontend::get_op_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            cur_state = frontend::State::IntLiteral;
            cur_str = input;
        }
        // 如果读入字符是字母，那么输出之前的运算符，然后转移到Ident状态
        else if(char_state == frontend::State::Ident){
            buf.type = frontend::get_op_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            cur_state = frontend::State::Ident;
            cur_str = input;
        }
        // 如果读入字符为"."，说明这是以小数点开头的浮点数，那么输出之前的运算符，然后转移到FloatLiteral状态
        else if(char_state == frontend::State::FloatLiteral){
            buf.type = frontend::get_op_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            cur_state = frontend::State::FloatLiteral;
            cur_str = input;
        }
        //如果为empty
        else if(char_state == frontend::State::Empty){
            buf.type = frontend::get_op_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            cur_state = frontend::State::Empty;
            cur_str = "";
        }
        // 如果读入字符是其他字符，那么输出之前的运算符，然后转移到Empty状态
        else{
            buf.type = frontend::get_op_TokenType(cur_str);
            buf.value = cur_str;
            ret = true;
            reset();
        }
    }
    
    
#ifdef DEBUG_DFA
    std::cout << "next state is [" << toString(cur_state) << "], next str = " << cur_str << "\t, ret = " << ret << std::endl;
#endif
    return ret;
}

// 重置有限状态机
void frontend::DFA::reset() {
    cur_state = State::Empty;
    cur_str = "";
}

frontend::Scanner::Scanner(std::string filename): fin(filename) {
    if(!fin.is_open()) {
        assert(0 && "in Scanner constructor, input file cannot open");
    }
}

frontend::Scanner::~Scanner() {
    fin.close();
}

// 预处理，去除注释
std::string frontend::Scanner::preprocess(std::string str, bool &passflag) {
    int size = str.size();
    std::string str2 = "";
    for(int i = 0; i < size; i++){
        // 如果识别到/*，那么就跳过注释
        if((str[i] == '/') &&  (i + 1 < size) && (str[i + 1] == '*')){
            passflag = true;
            i = i + 1;
            continue;
        }
        // 如果识别到*/，停止跳过注释
        if(str[i] == '*' &&  i + 1 < size && str[i+1] == '/'){
            passflag = false;
            i = i + 1;
            continue;
        }
        if(passflag) continue;
        // 如果识别到//，那么就跳过这一行
        if(str[i] == '/' && i + 1 < size && str[i + 1] == '/') break;
        str2 += str[i];
    }
    // std::cout << str2 << std::endl;
    return str2;
}

// 词法分析器的主函数
std::vector<frontend::Token> frontend::Scanner::run() {
    // 用于存储词法分析的结果
    std::vector<frontend::Token> ans; 
    // 用于存储当前读入的字符
    frontend::Token tk;
    // 状态机
    frontend::DFA dfa;
    // 用于存储当前读入的字符串
    std::string s;
    // 判断是否需要跳过
    bool passflag = false;
    // 检查是否到达文件末尾
    while(!fin.eof()){
        char charRead[20000];
        fin.getline(charRead,20000);
        std::string str(charRead);
        s += preprocess(str, passflag);
    }
    s += '\n';
    for(auto c : s){
        if(dfa.next(c, tk)){
            ans.push_back(tk);
        }
    }
    return ans;

#ifdef DEBUG_SCANNER
#include<iostream>
            std::cout << "token: " << toString(tk.type) << "\t" << tk.value << std::endl;
#endif
}

