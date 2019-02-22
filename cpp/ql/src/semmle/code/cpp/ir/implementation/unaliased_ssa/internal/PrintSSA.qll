private import SSAConstructionInternal
private import OldIR
private import Alias
private import SSAConstruction

/**
 * Property provide that dumps the memory access of each result. Useful for debugging SSA
 * construction.
 */
class PropertyProvider extends IRPropertyProvider {
  override string getInstructionProperty(Instruction instruction, string key) {
    exists(MemoryLocation location |
      location = getResultMemoryLocation(instruction) and
      (
        key = "ResultMemoryLocation" and result = location.toString() or
        key = "ResultVirtualVariable" and result = location.getVirtualVariable().toString()
      )
    )
    or
    exists(MemoryLocation location |
      location = getOperandMemoryLocation(instruction.getAnOperand()) and
      (
        key = "OperandMemoryAccess" and result = location.toString() or
        key = "OperandVirtualVariable" and result = location.getVirtualVariable().toString()
      )
    ) or
    exists(MemoryLocation useLocation, IRBlock defBlock, int defRank, int defIndex |
      hasDefinitionAtRank(useLocation, _, defBlock, defRank, defIndex) and
      defBlock.getInstruction(defIndex) = instruction and
      key = "DefinitionRank[" + useLocation.toString() + "]" and
      result = defRank.toString()
    ) or
    exists(MemoryLocation useLocation, IRBlock useBlock, int useRank |
      hasUseAtRank(useLocation, useBlock, useRank, instruction) and
      key = "UseRank[" + useLocation.toString() + "]" and
      result = useRank.toString()
    ) or
    exists(MemoryLocation useLocation, IRBlock defBlock, int defRank, int defIndex |
      hasDefinitionAtRank(useLocation, _, defBlock, defRank, defIndex) and
      defBlock.getInstruction(defIndex) = instruction and
      key = "DefinitionReachesUse[" + useLocation.toString() + "]" and
      result = strictconcat(IRBlock useBlock, int useRank, int useIndex |
        exists(Instruction useInstruction |
          hasUseAtRank(useLocation, useBlock, useRank, useInstruction) and
          useBlock.getInstruction(useIndex) = useInstruction and
          definitionReachesUse(useLocation, defBlock, defRank, useBlock, useRank)
        ) |
        useBlock.getDisplayIndex().toString() + "_" + useIndex, ", " order by useBlock.getDisplayIndex(), useIndex
      )
    )
  }

  override string getBlockProperty(IRBlock block, string key) {
    exists(MemoryLocation useLocation, int defRank, int defIndex |
      hasDefinitionAtRank(useLocation, _, block, defRank, defIndex) and
      (
        defIndex = -2 and key = "DefinitionRank(Phi:general)[" + useLocation.toString() + "]" + useLocation.getUniqueId() or
        defIndex = -1 and key = "DefinitionRank(Phi:specific)[" + useLocation.toString() + "]" + useLocation.getUniqueId()
      ) and
      result = defRank.toString()
    )
  }
}
