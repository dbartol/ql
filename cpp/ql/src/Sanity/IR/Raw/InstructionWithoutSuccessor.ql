/**
 * @name Instruction without successor (raw IR)
 * @description Sanity query to detect an instruction with no succesor.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/instruction-without-successor-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::instructionWithoutSuccessor(instr, message, func, funcText)
 select instr, message, func, funcText
