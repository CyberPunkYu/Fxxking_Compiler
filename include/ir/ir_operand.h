#ifndef IROPERAND_H
#define IROPERAND_H

#include <string>

// 不用TODO，这个文件不用改
// 该文件中​是对 IR 操作数的封装定义。我们对IR操作数进行类封装，
// 将其视为具有name、type属性的复杂数据类型。
// 这样将操作数绑定类型对于后续中端优化、后端处理等非常方便
//（毕竟类型系统也是语义分析一个重点）。

namespace ir {

enum class Type {
    Int,
    Float,
    IntLiteral,
    FloatLiteral,
    IntPtr,
    FloatPtr,
    null
};

std::string toString(Type t);

struct Operand {
    std::string name;
    Type type;
    Operand(std::string = "null", Type = Type::null);
};

}
#endif
