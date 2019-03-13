/**
 * @name Missing Phi operand (raw IR)
 * @description Sanity query to detect a missing Phi operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/missing-phi-operand-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::missingPhiOperand(instr, message, func, funcText)
 select instr, message, func, funcText
