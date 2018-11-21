/**
 * @name Missing Operand
 * @description Sanity query to detect missing operands in the IR
 * @id cpp/sanity/ir/raw/missing-operand
 * @kind problem
 */

import semmle.code.cpp.ir.implementation.raw.IR

from Instruction instr, string message, Function func, string funcName
where InstructionSanity::missingOperand(instr, message, func, funcName)
select instr, message, func, funcName
