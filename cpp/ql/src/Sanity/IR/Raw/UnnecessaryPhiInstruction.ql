/**
 * @name Unnecessary Phi instruction (raw IR)
 * @description Sanity query to detect a Phi instruction with fewer than two predecessor blocks.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/unnecessary-phi-instruction-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from PhiInstruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::unnecessaryPhiInstruction(instr, message, func, funcText)
 select instr, message, func, funcText
