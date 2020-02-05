/**
 */

import semmle.code.cpp.Print
import semmle.code.cpp.ir.implementation.unaliased_ssa.IR
import semmle.code.cpp.ir.implementation.unaliased_ssa.PrintIR
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasAnalysis
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasedSSA
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasConfiguration
import semmle.code.cpp.ir.implementation.IRConfiguration

class MyConfig extends PrintIRConfiguration {
  override predicate shouldPrintFunction(Function func) {
    getIdentityString(func) = "int UnreachableViaGoto()"
  }
}

/*
 * class Provider extends IRPropertyProvider {
 *  override string getInstructionProperty(Instruction instr, string key) {
 *    exists(Allocation var, int bitOffset |
 *      key = "ReadAllocationAndOffset" and
 *      addressOperandAllocationAndOffset(instr.getAnOperand().(MemoryOperand).getAddressOperand(),
 *        var, bitOffset) and
 *      result = var.toString() + "+" + bitOffset.toString()
 *    )
 *    or
 *    exists(Allocation var |
 *      key = "ReadAllocation" and
 *      var = getAddressOperandAllocation(instr.getAnOperand().(MemoryOperand).getAddressOperand()) and
 *      result = var.toString()
 *    )
 *  }
 * }
 */

query predicate locs(MemoryLocation def) { any() }

/*
from MemoryLocation def, MemoryLocation use
select def, use, getOverlap(def, use)
*/
import semmle.code.cpp.ir.IR as IR

from AliasedUseInstruction useInstr, MemoryOperand operand, AliasedDefinitionInstruction defInstr,
  MemoryLocation useLoc, MemoryLocation defLoc
where
  operand = useInstr.getAnOperand() and
  useLoc = getOperandMemoryLocation(operand) and
  defLoc = getResultMemoryLocation(defInstr)
select defInstr, defLoc, useInstr, useLoc, getOverlap(defLoc, useLoc)