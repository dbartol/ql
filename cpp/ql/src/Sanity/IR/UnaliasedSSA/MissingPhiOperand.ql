/**
 * @name Missing Phi operand (unaliased SSA IR)
 * @description Sanity query to detect a missing Phi operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/missing-phi-operand-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::missingPhiOperand(instr, message, func, funcText)
 select instr, message, func, funcText
