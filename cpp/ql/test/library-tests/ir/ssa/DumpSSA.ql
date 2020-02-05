/**
 * @kind graph
 */

import semmle.code.cpp.ir.implementation.unaliased_ssa.PrintIR
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.PrintSSA
import semmle.code.cpp.Print

class MyConfiguration extends PrintIRConfiguration {
  override predicate shouldPrintFunction(Function f) {
    getIdentityString(f) = "int Dereference(int*)"
  }
}
