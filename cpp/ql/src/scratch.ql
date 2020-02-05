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

/*
int getMetric(Allocation alloc) {
  result = count(Instruction instr | getResultMemoryLocation(instr).getAllocation() = alloc) *
    count(MemoryLocation loc | loc.getVirtualVariable().getAllocation() = alloc)
}
*/

newtype TRow = MkRow(MemoryLocation useLocation, MemoryLocation defLocation, IRBlock block, int rankIndex, int offset) {
  hasDefinitionAtRank(useLocation, defLocation, block, rankIndex, offset)
}

class Row extends MkRow {
  MemoryLocation useLocation;
  MemoryLocation defLocation;
  IRBlock block;
  int offset;

  Row() {
    this = MkRow(useLocation, defLocation, block, _, offset)
  }

  string toString() { result = "Row" }

  MemoryLocation getUseLocation() { result = useLocation }
  MemoryLocation getDefLocation() { result = defLocation }
  IRBlock getBlock() { result = block }
  int getOffset() { result = offset }
  IRFunction getIRFunction() { result = useLocation.getIRFunction() }
}

/*
from IRFunction irFunc, int defs
where
defs = count(Row row | row.getIRFunction() = irFunc | row)
select irFunc, getIdentityString(irFunc.getFunction()), defs order by defs desc
*/

query predicate expensive(IRFunction irFunc, string funcName, Allocation alloc) {
  isExpensiveAllocation(alloc) and alloc.getEnclosingIRFunction() = irFunc and funcName = getIdentityString(irFunc.getFunction())
}

/*
from MemoryLocation useLocation, int defs
where
  defs = strictcount(Row row | row.getUseLocation() = useLocation | row)
select useLocation, defs order by defs desc
*/

from IRFunction irFunc, int instrs, int defs, int locs, int metric, Allocation alloc, int allocMetric /* VirtualVariable largestVvar, int vvarMembers*/
where
instrs = count(irFunc.getAnInstruction()) and
defs = count(MemoryLocation useLocation, MemoryLocation defLocation, IRBlock block, int rankIndex,
    int offset |
    hasDefinitionAtRank(useLocation, defLocation, block, rankIndex, offset) and
    useLocation.getIRFunction() = irFunc
  ) and
locs = count(MemoryLocation loc | loc.getIRFunction() = irFunc) and
metric = getMetric(irFunc) and metric >= 40000 and
allocMetric = getAllocationMetric(alloc) and alloc.getEnclosingIRFunction() = irFunc

/*
largestVvar = max(VirtualVariable vvar | vvar.getIRFunction() = irFunc | vvar order by count(MemoryLocation loc | not loc.isReadOnly() and loc.getVirtualVariable() = vvar)) and
vvarMembers = count(MemoryLocation loc | not loc.isReadOnly() and loc.getVirtualVariable() = largestVvar)
*/
select irFunc, getIdentityString(irFunc.getFunction()), instrs, defs, metric, alloc, allocMetric order by allocMetric desc // vvarMembers, largestVvar order by defs desc


/*
 * from MemoryLocation loc
 * select loc
 */

/*
 * from IRBlock block, int rankIndex, int offset, MemoryLocation defLocation, int c
 * where
 *  c = strictcount(MemoryLocation useLocation | hasDefinitionAtRank(useLocation, defLocation, block, rankIndex, offset)) and
 *  c > 1000
 * select block.getFirstInstruction().getLocation(), offset, defLocation, c
 */

/*
from MemoryLocation useLocation, int c
where
  c =
    strictcount(MemoryLocation defLocation, IRBlock block, int offset |
      hasDefinitionAtRank(useLocation, defLocation, block, _, offset)
    )
select useLocation, c order by c desc
*/

query predicate sampleDefs(
  MemoryLocation defLocation, MemoryLocation useLocation, IRBlock block, int rankIndex, int offset/*,
  Overlap overlap*/
) {
  useLocation.toString() = "#string79:111[0..?)<unknown>" and
  hasDefinitionAtRank(useLocation, defLocation, block, rankIndex, offset) and
  not exists(getOverlap(defLocation, useLocation))
}

/*
 * from IRBlock block, int offset, AliasedVirtualVariable location
 * where hasDefinitionAtRank(location, location, block, _, offset)
 * select block.getDisplayIndex() as displayIndex, block.getLocation(), location, offset
 *
 * query predicate frontier(int dom, int front) {
 *  exists(IRBlock domBlock, IRBlock frontBlock |
 *    frontBlock = Dominance::getDominanceFrontier(domBlock) and
 *    dom = domBlock.getDisplayIndex() and
 *    front = frontBlock.getDisplayIndex()
 *  )
 * }
 */

/*
query predicate overlap(
  MemoryLocation defLocation, boolean canDef, MemoryLocation useLocation, boolean isRO,
  Overlap overlap
) {
  overlap = getOverlap(defLocation, useLocation) and
  not defLocation.toString().regexpMatch("^\\??\\#string.*") and
  (if defLocation.canDefineReadOnly() then canDef = true else canDef = false) and
  (if useLocation.isReadOnly() then isRO = true else isRO = false)
}

query predicate locs(MemoryLocation loc, string id) {
  (
    loc.toString() = "#string79:111[0..?)<unknown>" or
    loc.toString() = "{AllNonLocal}"
  ) and
  id = concat(loc.getAQlClass(), ", ")
}

query predicate stringVars(IRStringLiteral s, string t) {
  s.toString() = "#string79:111" and
  t = s.getAST().toString()
}

query predicate stringLits(Expr e, Function f, string loc) {
  exists(IRStringLiteral s |
    s.toString() = "#string79:111" and
    e = s.getAST() and
    f = e.getEnclosingFunction() and
    loc = e.getLocation().getFile().getAbsolutePath()
  )
}

query predicate trees(Element e) {
  e.getLocation().getFile().getAbsolutePath() = "/opt/src/Main/Include/definesvalidator.h" and
  e.getLocation().getStartLine() = 79
}

query predicate funcs(Function f) {
  getIdentityString(f) = "void definesvalidator::Validate()"
}
*/
/*
 * query predicate hdao(MemoryLocation defLoc, MemoryLocation useLoc, int blockIndex, int offset) {
 *  exists(IRBlock block |
 *    hasDefinitionAtRank(useLoc, defLoc, block, _, offset) and
 *    blockIndex = block.getDisplayIndex()
 *  )
 * }
 */

