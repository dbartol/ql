/**
 * @name Missing operand (aliased SSA IR)
 * @description Sanity query to detect a missing operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/missing-operand-aliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.aliased_ssa.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::missingOperand(instr, message, func, funcText)
 select instr, message, func, funcText
