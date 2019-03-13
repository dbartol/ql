/**
 * @name Unexplained loop (raw IR)
 * @description Sanity query to detect a loop that does not correspond to a loop in the AST.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/unexplained-loop-raw
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.raw.IR

 from IRBlock block, string message, FunctionIR func, string funcText
 where InstructionSanity::unexplainedLoop(block, message, func, funcText)
 select block, message, func, funcText
