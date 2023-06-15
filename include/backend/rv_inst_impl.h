#ifndef RV_INST_IMPL_H
#define RV_INST_IMPL_H

#include "backend/rv_def.h"

namespace rv {

struct rv_inst {
    rvREG rd, rs1, rs2;         // operands of rv inst
    rvOPCODE op;                // opcode of rv inst
    bool is_label;
    
    uint32_t imm;               // optional, in immediate inst
    std::string label;          // optional, in beq/jarl inst
    std::string symbol;         // optional, for la

    std::string draw() const;

    // constructors
    rv_inst();
    rv_inst(std::string label_name);
    rv_inst(rvOPCODE op, rvREG rd = rvREG::X0, rvREG rs1 = rvREG::X0, rvREG rs2 = rvREG::X0, uint32_t imm = 0);
};

};

#endif