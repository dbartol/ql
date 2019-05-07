/**
 * @name Ambiguous successors (Raw IR)
 * @description An IR instruction has multiple successors of the same kind.
 * @kind problem
 * @id cpp/sanity/ir/raw/ambiguous-successors
 * @problem.severity warning
 * @tags sanity
 */

import cpp
import semmle.code.cpp.ir.implementation.raw.IR
import semmle.code.cpp.ir.implementation.raw.IRSanity as IRSanity
import semmle.code.cpp.Print

from Instruction source, EdgeKind kind, int n, Instruction target
where IRSanity::ambiguousSuccessors(source, kind, n, target)
select source, "Instruction " + source.toString() + " has " + n.toString() + " successors of kind " + kind.toString() +
  " in function $@", source.getEnclosingFunction(), getIdentityString(source.getEnclosingFunction())
