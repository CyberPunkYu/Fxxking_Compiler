#include"backend/generator.h"

#include<assert.h>
#include<iostream>
#include<algorithm>
#include<iomanip>

using std::string;
using namespace rv;
using namespace ir;

#define INSERT(op, rd, rs1, rs2, imm) rv_insts->push_back(new rv_inst(op, rd, rs1, rs2, imm))


string rv::toString(rvREG r) {
    if(r == rvREG::X0) return "zero";
    if(r == rvREG::X1) return "ra";
    if(r == rvREG::X2) return "sp";
    if(r == rvREG::X3) return "gp";
    if(r == rvREG::X4) return "tp";
    if(r >= rvREG::X5 && r <= rvREG::X7) return "t" + std::to_string((int) r - 5);
    if(r == rvREG::X8) return "s0";
    if(r == rvREG::X9) return "s1";
    if(r >= rvREG::X10 && r <= rvREG::X17) return "a" + std::to_string((int) r - 10);
    if(r >= rvREG::X18 && r <= rvREG::X27) return "s" + std::to_string((int) r - 16);
    if(r >= rvREG::X28 && r <= rvREG::X31) return "t" + std::to_string((int) r - 25);
    assert(0 && "invalid r");
}

string rv::toString(rvOPCODE op) {
    switch(op) {
    case rvOPCODE::ADD: return "add";
    case rvOPCODE::SUB: return "sub";
    case rvOPCODE::XOR: return "xor";
    case rvOPCODE::OR: return "or";
    case rvOPCODE::AND: return "and";
    case rvOPCODE::SLL: return "sll";
    case rvOPCODE::SRL: return "srl";
    case rvOPCODE::SRA: return "sra";
    case rvOPCODE::SLT: return "slt";
    case rvOPCODE::SLTU: return "sltu";
    case rvOPCODE::MUL: return "mul";
    case rvOPCODE::DIV: return "div";
    case rvOPCODE::REM: return "rem";
    case rvOPCODE::ADDI: return "addi";
    case rvOPCODE::XORI: return "xori";
    case rvOPCODE::ORI: return "ori";
    case rvOPCODE::ANDI: return "andi";
    case rvOPCODE::SLLI: return "slli";
    case rvOPCODE::SRLI: return "srli";
    case rvOPCODE::SRAI: return "srai";
    case rvOPCODE::SLTI: return "slti";
    case rvOPCODE::SLTIU: return "sltiu";
    case rvOPCODE::SEQZ: return "seqz";
    case rvOPCODE::SNEZ: return "snez";
    case rvOPCODE::SLTZ: return "sltz";
    case rvOPCODE::SGTZ: return "sgtz";
    case rvOPCODE::LW: return "lw";
    case rvOPCODE::SW: return "sw";
    case rvOPCODE::BEQ: return "beq";
    case rvOPCODE::BNE: return "bne";
    case rvOPCODE::BLT: return "blt";
    case rvOPCODE::BGE: return "bge";
    case rvOPCODE::BLTU: return "bltu";
    case rvOPCODE::BGEU: return "bgeu";
    case rvOPCODE::BNEZ: return "bnez";
    case rvOPCODE::LA: return "la";
    case rvOPCODE::LI: return "li";
    case rvOPCODE::MOV: return "mv";
    case rvOPCODE::J: return "j";
    case rvOPCODE::JR: return "jr";
    case rvOPCODE::CALL: return "call";
    default:
        assert(0);
        break;
    }
}

string rv::rv_inst::draw() const {
    string res;
    res += toString(op) + " ";
    switch(op) {
        // op rd, rs1, rs2
        case rvOPCODE::ADD:
        case rvOPCODE::SUB:
        case rvOPCODE::XOR:
        case rvOPCODE::OR:
        case rvOPCODE::AND:
        case rvOPCODE::SLL:
        case rvOPCODE::SRL:
        case rvOPCODE::SRA:
        case rvOPCODE::SLT:
        case rvOPCODE::SLTU:
        case rvOPCODE::MUL:
        case rvOPCODE::DIV:
        case rvOPCODE::REM:
            res += toString(rd) + ", " + toString(rs1) + ", " + toString(rs2);
            break;
        // op rd, rs1, imm
        case rvOPCODE::ADDI:
        case rvOPCODE::XORI:
        case rvOPCODE::ORI:
        case rvOPCODE::ANDI:
        case rvOPCODE::SLLI:
        case rvOPCODE::SRLI:
        case rvOPCODE::SRAI:
        case rvOPCODE::SLTI:
        case rvOPCODE::SLTIU:
            res += toString(rd) + ", " + toString(rs1) + ", " + std::to_string((int) imm);
            break;
        // op rd, rs1
        case rvOPCODE::SEQZ: // rd = (rs1 == 0)
        case rvOPCODE::SNEZ: // rd = (rs1 != 0)
        case rvOPCODE::SLTZ: // rd = (rs1  < 0)
        case rvOPCODE::SGTZ: // rd = (rs1  > 0)
            res += toString(rd) + ", " + toString(rs1);
            break;
        // op rd, imm(rs1)
        case rvOPCODE::LW:
            if(imm == INT32_MAX) { res += toString(rd) + ", " + symbol; }
            else { res += toString(rd) + ", " + std::to_string((int) imm) + "(" + toString(rs1) + ")"; }
            break;
        // op rs2, imm(rs1)
        case rvOPCODE::SW:
            if(imm == INT32_MAX) { res += toString(rs2) + ", " + symbol + ", " + toString(rvREG::X31); }
            else { res += toString(rs2) + ", " + std::to_string((int) imm) + "(" + toString(rs1) + ")"; }
            break;
        // op rs1, rs2, label
        case rvOPCODE::BEQ:
        case rvOPCODE::BNE:
        case rvOPCODE::BLT:
        case rvOPCODE::BGE:
        case rvOPCODE::BLTU:
        case rvOPCODE::BGEU:
            res += toString(rs1) + ", " + toString(rs2) + ", " + label;
            break;
        // op rs1, label
        case rvOPCODE::BNEZ: // rs1 != 0
            res += toString(rs1) + ", " + label;
            break;

        case rvOPCODE::LA:
            res += toString(rd) + ", " + symbol;
            break;
        case rvOPCODE::LI:
            res += toString(rd) + ", " + std::to_string((int) imm);
            break;
        case rvOPCODE::MOV:
            res += toString(rd) + ", " + toString(rs1);
            break;
        case rvOPCODE::J:
            res += label;
            break;
        case rvOPCODE::JR:
            res += toString(rs1);
            break;
        case rvOPCODE::CALL:
            res += label;
            break;
        default: assert(0); break;
    }
    return res;
}

rv::rv_inst::rv_inst(): is_label(false) {}
rv::rv_inst::rv_inst(string label_name): is_label(true), label(label_name) {}
rv::rv_inst::rv_inst(rvOPCODE _op, rvREG _rd, rvREG _rs1, rvREG _rs2, uint32_t _imm): op(_op), rd(_rd), rs1(_rs1), rs2(_rs2), imm(_imm), is_label(false) {}

bool backend::operandCmp::operator() (const ir::Operand& a, const ir::Operand& b) const { return a.name < b.name; }
backend::stackVarMap::stackVarMap(const std::vector<ir::GlobalVal> &gb): offset(-16), globalVal(gb) {}

int backend::stackVarMap::add_operand(Operand op, uint32_t size) { 
    offset -= size;
    _table[op] = offset;
    return offset;
}

int backend::stackVarMap::find_operand(Operand op) {
    auto it = _table.find(op);
    if(it == _table.end()) {
        for(GlobalVal gv : globalVal) 
            if(gv.val.name == op.name) { return INT32_MAX; }
        return add_operand(op);
    }
    else { return it->second; }
}

backend::regAllocator::regAllocator(std::vector<rv::rv_inst*> &rv_insts_out, const std::vector<ir::GlobalVal> &globalVal_out): reg2op_map(32), reg_timestamp(32), rv_insts(rv_insts_out), stack_var_map(globalVal_out) {
    available_regs.insert(rvREG::X5);
    available_regs.insert(rvREG::X6);
    available_regs.insert(rvREG::X7);
    for(int i = 10; i <= 17; i++) {
        available_regs.insert((rvREG) i);
    }
}


void backend::regAllocator::update(rv::rvREG r, int time) {
    auto it = reg_using.find(std::make_pair(reg_timestamp[(int) r], r));
    reg_using.erase(it);
    reg_timestamp[(int) r] = time;
    reg_using.emplace(time, r);
}

void backend::regAllocator::spill(rvREG r) {
    Operand operand = reg2op_map[(int) r];
    if(operand.name == "null") { return; }
    int pos = stack_var_map.find_operand(operand);

    rv_inst* store_inst = new rv_inst();
    store_inst->op = rvOPCODE::SW;
    store_inst->rs1 = rvREG::X8;
    store_inst->rs2 = r;
    store_inst->imm = pos;
    if(pos == INT32_MAX) {
        store_inst->symbol = operand.name;
        // 如果是全局变量 且 是指针，才不用加rv_insts sw
    }
    // sw r, pos(s0)
    if(!(pos == INT32_MAX && operand.type == Type::IntPtr)) {
        rv_insts.push_back(store_inst);
    }

    available_regs.insert(r);
    op2reg_map.erase(op2reg_map.find(operand));
    reg_using.erase(reg_using.find({reg_timestamp[(int) r], r}));
    reg2op_map[(int) r] = Operand();
}

void backend::regAllocator::load(rvREG r, ir::Operand op, int time, int needload) {
    available_regs.erase(r);
    reg_using.emplace(reg_timestamp[(int) r], r);
    update(r, time);

    if(op2reg_map.count(op)) {
        rv_inst* mv = new rv_inst();
        mv->op = rvOPCODE::MOV;
        mv->rs1 = op2reg_map[op];
        mv->rd = r;
        if(needload) {
            rv_insts.push_back(mv);
        }

        available_regs.insert(mv->rs1);
        op2reg_map[op] = r;
        reg_using.erase(reg_using.find({reg_timestamp[(int) mv->rs1], mv->rs1}));
        reg2op_map[(int) mv->rs1] = Operand();
        reg2op_map[(int) r] = op;
    } else {
        int pos = stack_var_map.find_operand(op);
        op2reg_map[op] = r;
        reg2op_map[(int) r] = op;

        rv_inst* load = new rv_inst();
        load->op = rvOPCODE::LW;
        load->rs1 = rvREG::X8;
        load->imm = pos;
        load->rd = r;
        if(pos == INT32_MAX) {
            load->symbol = op.name;
            if(op.type == Type::IntPtr || op.type == Type::FloatPtr) {
                load->op = rvOPCODE::LA;
            }
        }
        if(needload) {
            rv_insts.push_back(load);
        }
    }
}

rvREG backend::regAllocator::getReg(Operand op, int time, int needload) {
    // first check whether op has a reg
    if(op2reg_map.count(op)) {
        // op has reg, return reg belong to it
        rvREG r = op2reg_map[op];
        update(r, time);
        return r;
    }
    if(available_regs.size()) {
        // have available regs, directly alloc available reg to op
        rvREG r = *available_regs.begin();
        load(r, op, time, needload);
        return r;
    }
    rvREG r;
    int last_time;
    std::tie(last_time, r) = *reg_using.begin();
    spill(r);

    load(r, op, time, needload);
    return r;
}

void backend::regAllocator::clearregs() {
    op2reg_map.clear();
    reg2op_map = std::vector<ir::Operand>(32);
    available_regs.clear();

    available_regs.insert(rvREG::X5);
    available_regs.insert(rvREG::X6);
    available_regs.insert(rvREG::X7);
    for(int i = 10; i <= 17; i++) {
        available_regs.insert((rvREG) i);
    }
    reg_using.clear();
    reg_timestamp = std::vector<int>(32);
}

// gen
backend::Generator::Generator(ir::Program& p, std::ofstream& f): program(p), fout(f), global_label_id(0) {}

int backend::Generator::get_label_id(int line_num) {
    auto it = std::lower_bound(goto_label_lines->begin(), goto_label_lines->end(), line_num);
    if(it == goto_label_lines->end() || *it != line_num) return -1;
    return (int) (it - goto_label_lines->begin() + global_label_id);
}

void backend::Generator::gen() {
    fout << "\t.text\n";
    for(auto i : program.globalVal) { fout << "\t.comm\t" + i.val.name + "," + std::to_string(std::max(1, i.maxlen) * 4) + ",4" + "\n"; }
    for(auto i : program.functions) { gen_func(i); }
}

void backend::Generator::gen_func(const Function& func) {
    fout << "\t.align\t1\n\t.global\t" + func.name + "\n\t.type\t" + func.name + ", @function\n" + func.name + ":\n";
    rv_insts = new std::vector<rv_inst*>();
    reg_allocator = new regAllocator(*rv_insts, program.globalVal);
    goto_label_lines = new std::vector<int>();
    auto &ngoto_label_lines = *goto_label_lines;
    ret_set = new std::set<rv_inst*>();

    // abi get func parameters
    int apr = 10, fapr = 10, stackpr = 0;
    for(int i = 0; i <  (int) func.ParameterList.size(); i++) {
        Operand op = func.ParameterList[i];
        reg_allocator->reg2op_map[apr] = op;
        reg_allocator->op2reg_map[op] = (rvREG) apr;
        reg_allocator->available_regs.erase((rvREG) apr);
        reg_allocator->reg_using.emplace(reg_allocator->reg_timestamp[apr], (rvREG) apr);
        apr++;
    }
    rv_inst *spsub_inst = new rv_inst(rvOPCODE::ADDI, rvREG::X2, rvREG::X2, rvREG::X0);
    rv_inst *svra_inst = new rv_inst(rvOPCODE::SW, rvREG::X0, rvREG::X2, rvREG::X1);
    rv_inst *svs0_inst = new rv_inst(rvOPCODE::SW, rvREG::X0, rvREG::X2, rvREG::X8);
    rv_inst *spadd_inst = new rv_inst(rvOPCODE::ADDI, rvREG::X8, rvREG::X2, rvREG::X0);
    rv_insts->push_back(spsub_inst);
    rv_insts->push_back(svra_inst);
    rv_insts->push_back(svs0_inst);
    rv_insts->push_back(spadd_inst);

    for(int i = 0; i <  (int) func.InstVec.size(); i++) {
        Instruction* inst = func.InstVec[i];
        if(inst->op == Operator::_goto) { ngoto_label_lines.push_back(i + stoi(inst->des.name)); }
    }
    std::sort(ngoto_label_lines.begin(), ngoto_label_lines.end());
    ngoto_label_lines.erase(std::unique(ngoto_label_lines.begin(), ngoto_label_lines.end()), ngoto_label_lines.end());
    
    // gen insts
    max_call_overflow_paras = 0;
    for(int i = 0; i <  (int) func.InstVec.size(); i++) {
        int label_id = -1;
        if((label_id = get_label_id(i)) != -1) {
            // store all temp
            std::vector<rvREG> store_regs;
            for(auto reg_info : reg_allocator->reg_using) { store_regs.push_back(reg_info.second); }
            for(auto reg : store_regs) { reg_allocator->spill(reg); }
            rv_insts->push_back(new rv_inst(".L" + std::to_string(label_id)));
            reg_allocator->clearregs();
        }
        Instruction* inst = func.InstVec[i];
        gen_instr(*inst, i);
    }
    // write back
    if((reg_allocator->stack_var_map.offset - max_call_overflow_paras * 4) % 8 != 0) {
        max_call_overflow_paras++;
    }
    spadd_inst->imm = -(spsub_inst->imm = reg_allocator->stack_var_map.offset - max_call_overflow_paras * 4);
    svra_inst->imm = spadd_inst->imm - 4;
    svs0_inst->imm = spadd_inst->imm - 8;

    for(rv_inst* reti : *ret_set) {
        if(reti->rd == rvREG::X2) { reti->imm = spadd_inst->imm; }
        else if(reti->rd == rvREG::X8) { reti->imm = spadd_inst->imm - 8; }
        else { reti->imm = spadd_inst->imm - 4; }
    }

    for(int i = 0; i <  (int) rv_insts->size(); i++) {
        if((*rv_insts)[i]->is_label) { fout << (*rv_insts)[i]->label << ":\n"; }
        else{ fout << "\t" << (*rv_insts)[i]->draw() << "\n"; }
    }

    fout << "\t.size\t" + func.name + ", .-" + func.name + "\n";
    global_label_id += ngoto_label_lines.size();
    delete rv_insts;
    delete reg_allocator;
    delete goto_label_lines;
    delete ret_set;
}

void backend::Generator::gen_instr(const Instruction& inst, int time) {
    switch(inst.op) {
        case Operator::alloc: {
            reg_allocator->stack_var_map.offset -= stoi(inst.op1.name) * 4;
            INSERT(rvOPCODE::ADDI, reg_allocator->getReg(inst.des, time, 0), rvREG::X8, rvREG::X0, reg_allocator->stack_var_map.offset + 4);
        } break;
        case Operator::load: {
            INSERT(rvOPCODE::MOV, rvREG::X31, reg_allocator->getReg(inst.op2, time, 1), rvREG::X0, 0);//mv t6, op2
            INSERT(rvOPCODE::SLLI, rvREG::X31, rvREG::X31, rvREG::X0, 2);//sll t6, t6, 2
            INSERT(rvOPCODE::ADD, rvREG::X31, rvREG::X31, reg_allocator->getReg(inst.op1, time, 1), 1);//add t6, t6, op1
            INSERT(rvOPCODE::LW, reg_allocator->getReg(inst.des, time, 0), rvREG::X31, rvREG::X0, 0);//lw des, 0(t6)
        } break;
        case Operator::store: {
            if(inst.op2.type == Type::Int) {
                INSERT(rvOPCODE::MOV, rvREG::X31, reg_allocator->getReg(inst.op2, time, 1), rvREG::X0, 0);//mv t6, op2
                INSERT(rvOPCODE::SLLI, rvREG::X31, rvREG::X31, rvREG::X0, 2);//sll t6, t6, 2
                INSERT(rvOPCODE::ADD, rvREG::X31, rvREG::X31, reg_allocator->getReg(inst.op1, time, 1), 0);//add t6, t6, op1
                INSERT(rvOPCODE::SW, rvREG::X0, rvREG::X31, reg_allocator->getReg(inst.des, time, 1), 0);//lw des, 0(t6)
            } else{ INSERT(rvOPCODE::SW, rvREG::X0, reg_allocator->getReg(inst.op1, time, 1), reg_allocator->getReg(inst.des, time, 1), stoi(inst.op2.name) * 4); }
        } break;
        case Operator::mov:
        case Operator::def: {
            if(inst.op1.type == Type::Int) { INSERT(rvOPCODE::MOV, reg_allocator->getReg(inst.des, time, 0), reg_allocator->getReg(inst.op1, time, 1), rvREG::X0, 0); }
            else {INSERT(rvOPCODE::LI, reg_allocator->getReg(inst.des, time, 0), rvREG::X0, rvREG::X0, stoi(inst.op1.name)); }
        } break;
        case Operator::mul:
        case Operator::div:
        case Operator::mod:
        case Operator::sub:
        case Operator::add: {
            rvOPCODE op;
            switch(inst.op) {
                case Operator::add: op = rvOPCODE::ADD; break;
                case Operator::sub: op = rvOPCODE::SUB; break;
                case Operator::mul: op = rvOPCODE::MUL; break;
                case Operator::div: op = rvOPCODE::DIV; break;
                case Operator::mod: op = rvOPCODE::REM; break;
                default:
                    assert(0);
                    break;
            }
            INSERT(op, reg_allocator->getReg(inst.des, time, 0), reg_allocator->getReg(inst.op1, time, 1), reg_allocator->getReg(inst.op2, time, 1), 0);
        } break; 
        case Operator::_return: {
            // store all temp
            std::vector<rvREG> store_regs;
            for(auto reg_info : reg_allocator->reg_using) {
                if(reg_allocator->stack_var_map.find_operand(reg_allocator->reg2op_map[(int) reg_info.second]) == INT32_MAX) {
                    store_regs.push_back(reg_info.second);
                }
            }
            for(auto reg : store_regs) { reg_allocator->spill(reg); }
            if(inst.op1.type == Type::Int) {
                reg_allocator->spill(rvREG::X10);
                reg_allocator->load(rvREG::X10, inst.op1, time, 1);
            }
            else if(inst.op1.type == Type::IntLiteral) {
                reg_allocator->spill(rvREG::X10);
                INSERT(rvOPCODE::LI, rvREG::X10, rvREG::X0, rvREG::X0, stoi(inst.op1.name));
            }
            // save s0
            rv_inst *lds0_inst = new rv_inst(rvOPCODE::LW, rvREG::X8, rvREG::X2, rvREG::X0);
            rv_insts->push_back(lds0_inst);
            ret_set->insert(lds0_inst);
            // load ra
            rv_inst *ldra_inst = new rv_inst(rvOPCODE::LW, rvREG::X1, rvREG::X2, rvREG::X0);
            rv_insts->push_back(ldra_inst);
            ret_set->insert(ldra_inst);
            // s0 = sp + frame size
            rv_inst *spadd_inst = new rv_inst(rvOPCODE::ADDI, rvREG::X2, rvREG::X2, rvREG::X0);
            rv_insts->push_back(spadd_inst);
            ret_set->insert(spadd_inst);
            // jr ra
            INSERT(rvOPCODE::JR, rvREG::X0, rvREG::X1, rvREG::X0, 0);
        } break;
        case Operator::_goto: {
            
            rvOPCODE op; rvREG rs1;
            if(inst.op1.name == "null") {
                op = rvOPCODE::J;
            } else {
                op = rvOPCODE::BNEZ;
                rs1 = reg_allocator->getReg(inst.op1, time, 0);
            }
            rv_inst *goto_inst = new rv_inst(op, rvREG::X0, rs1, rvREG::X0, 0);
            goto_inst->label = ".L" + std::to_string(get_label_id(time + stoi(inst.des.name)));
            // store all temp
            std::vector<rvREG> store_regs;
            for(auto reg_info : reg_allocator->reg_using) { store_regs.push_back(reg_info.second); }
            for(auto reg : store_regs) { reg_allocator->spill(reg); }
            rv_insts->push_back(goto_inst);
        } break;
        case Operator::call: {
            CallInst *callinst = (CallInst*) &inst;
            int intcnt = 0, floatcnt = 0;
            for(Operand op : callinst->argumentList) {
                if(op.type == Type::Int) intcnt++;
                else floatcnt++;
            }
            max_call_overflow_paras = std::max(max_call_overflow_paras, std::max(intcnt - 8, 0) + std::max(floatcnt - 8, 0));
            // store all temp
            std::vector<rvREG> store_regs;
            for(auto reg_info : reg_allocator->reg_using) { store_regs.push_back(reg_info.second); }
            for(auto reg : store_regs) { reg_allocator->spill(reg); }
            // load arguments to a0~a7
            // abi get func parameters
            int apr = 10;
            for(int i = 0; i <  (int) callinst->argumentList.size(); i++) {
                Operand op = callinst->argumentList[i];
                reg_allocator->load((rvREG) apr, op, time, 1);
                apr++;
            }
            // call function
            rv_inst* call_inst = new rv_inst(rvOPCODE::CALL);
            call_inst->label = inst.op1.name;
            rv_insts->push_back(call_inst);
            // process return val
            Function calledFunction;
            int foundFunction = 0;
            for(Function f : program.functions) {
                if(f.name == inst.op1.name) {
                    calledFunction = f;
                    foundFunction = 1;
                    break;
                }
            }
            string funcnames = inst.op1.name;
            if(!foundFunction) {
                calledFunction = *frontend::get_lib_funcs()->find(inst.op1.name)->second;
            }
            reg_allocator->clearregs();
            if(calledFunction.returnType == Type::Int) {
                INSERT(rvOPCODE::MOV, reg_allocator->getReg(inst.des, time, 0), rvREG::X10, rvREG::X0, 0);
            }
        } break;

        case Operator::neq:
        case Operator::lss:
        case Operator::gtr:
        case Operator::eq: {
            rvOPCODE op;
            rv_inst *op_inst = new rv_inst();
            switch(inst.op) {
                case Operator::neq: op = rvOPCODE::SNEZ; break;
                case Operator::lss: op = rvOPCODE::SLTZ; break;
                case Operator::gtr: op = rvOPCODE::SGTZ; break;
                case Operator::eq:  op = rvOPCODE::SEQZ; break;
                default:
                    assert(0);
                    break;
            }
            INSERT(op, reg_allocator->getReg(inst.des, time, 0), reg_allocator->getReg(inst.op1, time, 1), rvREG::X0, 0);
        } break;
        default: assert(0); break;
    }
}