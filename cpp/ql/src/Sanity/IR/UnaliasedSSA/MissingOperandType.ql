/**
 * @name Missing operand type (unaliased SSA IR)
 * @description Sanity query to detect a missing type on an operand.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/missing-operand-type-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from Operand operand, string message, FunctionIR func, string funcText
 where InstructionSanity::missingOperandType(operand, message, func, funcText)
 select operand, message, func, funcText
