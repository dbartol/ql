import cpp
import semmle.code.cpp.ir.IR
import semmle.code.cpp.Print

select
  "reg: " + count(RegisterOperand operand | any()).toString(),
  "mem: " + count(MemoryOperand operand | any()).toString(),
  "chidef: " + count(MemoryOperand operand |
    operand.getDefinitionInstruction() instanceof ChiInstruction
  ).toString(),
  "~def:" + count(MemoryOperand operand |
    not operand.isDefinitionExact()
  ).toString(),
  "exactdef: " + count(MemoryOperand operand |
    operand.isDefinitionExact()
  ).toString(),
  "better_than_chi: " + count(MemoryOperand operand |
    operand.getDefinitionInstruction().getASuccessor() instanceof ChiInstruction and
    not operand instanceof ChiPartialOperand
  ).toString()
/*
from FunctionIR func, int preciseCount
where preciseCount = strictcount(MemoryOperand operand |
  operand.getEnclosingFunctionIR() = func and
  operand.getDefinitionInstruction().getASuccessor() instanceof ChiInstruction
)
select getIdentityString(func.getFunction()), preciseCount order by preciseCount
*/