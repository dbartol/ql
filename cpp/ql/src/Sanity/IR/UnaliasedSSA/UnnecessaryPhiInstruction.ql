/**
 * @name Unnecessary Phi instruction (unaliased SSA IR)
 * @description Sanity query to detect a Phi instruction with fewer than two predecessor blocks.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/unnecessary-phi-instruction-unaliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.unaliased_ssa.IR

 from PhiInstruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::unnecessaryPhiInstruction(instr, message, func, funcText)
 select instr, message, func, funcText
