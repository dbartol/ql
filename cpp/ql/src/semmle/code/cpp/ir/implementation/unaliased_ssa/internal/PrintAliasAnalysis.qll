private import AliasAnalysis
private import AliasAnalysisInternal
private import AliasAnalysisImports
private import InputIR

/**
 * Property provide that dumps the alias analysis data for each result and operand. Useful for
 * debugging.
 */
private class PropertyProvider extends IRPropertyProvider {
  override string getInstructionProperty(Instruction instruction, string key) {
    exists(
      AddressOperand addrOperand, Configuration::Allocation allocation, Ints::IntValue bitOffset
    |
      addrOperand.getUse() = instruction and
      key = "AddressOperandPointsTo" and
      addressOperandAllocationAndOffset(addrOperand, allocation, bitOffset) and
      result = allocation.toString() + ":" + Ints::bitsToBytesAndBits(bitOffset)
    )
  }
}
