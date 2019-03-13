/**
 * @name Unexplained loop (aliased SSA IR)
 * @description Sanity query to detect a loop that does not correspond to a loop in the AST.
 * @kind problem
 * @problem.severity error
 * @precision high
 * @id cpp/unexplained-loop-aliased-ssa
 * @tags sanity
 */

 import semmle.code.cpp.ir.implementation.aliased_ssa.IR

 from IRBlock block, string message, FunctionIR func, string funcText
 where InstructionSanity::unexplainedLoop(block, message, func, funcText)
 select block, message, func, funcText
