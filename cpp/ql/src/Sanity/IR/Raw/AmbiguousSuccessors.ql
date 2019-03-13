/**
 * @name Ambiguous successors (raw IR)
 * @description Sanity query to detect an instruction with multiple successors of the same kind.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/ambiguous-successors-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::ambiguousSuccessors(instr, message, func, funcText)
 select instr, message, func, funcText
