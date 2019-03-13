/**
 * @name Missing operand (raw IR)
 * @description Sanity query to detect a missing operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/missing-operand-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::missingOperand(instr, message, func, funcText)
 select instr, message, func, funcText
