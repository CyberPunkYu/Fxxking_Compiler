# Fxxking_Compiler


## 基本操作
1. compiler [src_filename] -s2 -o [output_filename] 将输出你的 IR 程序至 [output_filename]
用于观察自己的 IR 是否生成正确
2. compiler [src_filename] -e -o [output_filename] 将执行你的 IR 程序并输出其结果到 [output_filename]
包括源程序调用 putint 等函数输出到标准输出的内容 以及将程序 main 函数的返回值打印到最后一行，这个才是用于测试比对的


## 注意事项
1. IR .h文件被放入静态库中，我们不能修改它，只能在自己的代码中调用它
2. 对于部分测试用例会出现getch或者getint，其中输入数据需要单独放到文件中，后缀为.in（test中应该存在）
3. 四元式为（opcode，des，operand1，operand2）
opcode 代表 IR 的种类决定了 IR 功能，des 是目的操作数，operand1 & operand2 是源操作数
4. 指针操作数被认为是整型
5. 算了，太多了写不过来


## 我的理解（基于文档）
1. 根本目的：语义分析＋IR生成
2. .out文件包含两部分，第一行是putint等函数输出到标准输出的内容，第二行是main函数的返回值
例如
int main() {
    putint(100); putch(32); putch(97);
    return 3;
}
则.out文件为
100 a(32 -> 空格, 97 -> a)
3
3. IR类型为变量赋值、算术运算、逻辑运算、访存运算、类型转化、跳转；与Risc-v指令集对应
除此之外还有变量定义、调用返回、指针运算
IR 具体转换看IR定义文档


