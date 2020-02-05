/**
 * @kind graph
 */

import semmle.code.cpp.ir.implementation.unaliased_ssa.IR
import semmle.code.cpp.ir.implementation.unaliased_ssa.PrintIR
import semmle.code.cpp.ir.IRConfiguration
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.SSAConstruction
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasedSSA
import semmle.code.cpp.Print
import DefUse

class MyConfiguration extends PrintIRConfiguration {
  override predicate shouldPrintFunction(Function f) {
    getIdentityString(f) = "void definesvalidator::Validate()"
  }
}
