import semmle.code.cpp.ir.implementation.unaliased_ssa.IR
import semmle.code.cpp.ir.IRConfiguration
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.SSAConstruction
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasedSSA
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasConfiguration
import semmle.code.cpp.Print
//import semmle.code.cpp.ir.implementation.unaliased_ssa.internal.reachability.Dominance as Dominance
import DefUse

class MyConfiguration extends IRConfiguration {
  override predicate shouldCreateIRForFunction(Function f) {
//    getIdentityString(f) = "void definesvalidator::Validate()"
    any()
  }
}

query predicate expensive(IRFunction irFunc, string funcName, Allocation alloc) {
  isExpensiveAllocation(alloc) and alloc.getEnclosingIRFunction() = irFunc and funcName = getIdentityString(irFunc.getFunction())
}
