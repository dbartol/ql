/**
 * @name Ambiguous successors (unaliased SSA IR)
 * @description Sanity query to detect an instruction with multiple successors of the same kind.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/ambiguous-successors-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::ambiguousSuccessors(instr, message, func, funcText)
 select instr, message, func, funcText
