/**
 * @name Instruction without successor (unaliased SSA IR)
 * @description Sanity query to detect an instruction with no succesor.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/instruction-without-successor-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::instructionWithoutSuccessor(instr, message, func, funcText)
 select instr, message, func, funcText
