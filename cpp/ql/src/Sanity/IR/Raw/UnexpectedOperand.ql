/**
 * @name Unexpected operand (raw IR)
 * @description Sanity query to detect an unexpected operand on an instruction.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/unexpected-operand-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from Instruction instr, string message, FunctionIR func, string funcText
 where InstructionSanity::unexpectedOperand(instr, message, func, funcText)
 select instr, message, func, funcText
