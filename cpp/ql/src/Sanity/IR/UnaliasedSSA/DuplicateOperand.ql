/**
 * @name Duplicate operand (unaliased SSA IR)
 * @description Sanity query to detect a duplicate operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/duplicate-operand-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::duplicateOperand(instr, message, func, funcText)
 select instr, message, func, funcText
