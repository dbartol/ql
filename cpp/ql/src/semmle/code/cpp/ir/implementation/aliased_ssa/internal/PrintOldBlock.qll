private import SSAConstructionInternal
private import NewIR
private import SSAConstruction
private import DebugSSA

private class OldBlock = Reachability::ReachableBlock;

/**
 * Property provide that dumps the ID of the previous `IRBlock`(s) that contributed to an `IRBlock`.
 */
class PropertyProvider extends IRPropertyProvider {
  override string getBlockProperty(IRBlock block, string key) {
    key = "OldBlock" and
    result = concat (getOldInstruction(block.getAnInstruction()).getBlock().getDisplayIndex().toString(), ", ")
  }
}
