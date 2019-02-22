import SSAConstructionInternal
import cpp
private import semmle.code.cpp.ir.implementation.Opcode
private import semmle.code.cpp.ir.internal.OperandTag
private import semmle.code.cpp.ir.internal.Overlap
private import NewIR

private class OldBlock = Reachability::ReachableBlock;
private class OldInstruction = Reachability::ReachableInstruction;

import Cached
cached private module Cached {

  private IRBlock getNewBlock(OldBlock oldBlock) {
    result.getFirstInstruction() = getNewInstruction(oldBlock.getFirstInstruction())
  }

  cached predicate functionHasIR(Function func) {
    exists(OldIR::FunctionIR funcIR |
      funcIR.getFunction() = func
    )
  }

  cached OldInstruction getOldInstruction(Instruction instr) {
    instr = WrappedInstruction(result)
  }

  private Instruction getNewInstruction(OldInstruction instr) {
    getOldInstruction(result) = instr
  }

  /**
   * Gets the chi node corresponding to `instr` if one is present, or the new `Instruction`
   * corresponding to `instr` if there is no `Chi` node.
   */
  private Instruction getNewFinalInstruction(OldInstruction instr) {
    result = Chi(instr)
    or
    not exists(Chi(instr)) and
    result = getNewInstruction(instr)
  }

  pragma[inline]
  bindingset[defIndex, defLocation]
  private Instruction getDefinitionOrChiInstruction(OldBlock defBlock, int defIndex, Alias::MemoryLocation defLocation,
      Overlap overlap) {
    (
      defIndex >= 0 and
      exists(OldInstruction oldInstr |
        oldInstr = defBlock.getInstruction(defIndex) and
        if overlap instanceof MayPartiallyOverlap then
          result = Chi(oldInstr)
        else
          result = getNewInstruction(oldInstr)
      )
    ) or
    (
      defIndex < 0 and
      result = Phi(defBlock, defLocation)
    )
  }

  private IRVariable getNewIRVariable(OldIR::IRVariable var) {
    // This is just a type cast. Both classes derive from the same newtype.
    result = var
  }

  cached newtype TInstruction =
    WrappedInstruction(OldInstruction oldInstruction) {
      not oldInstruction instanceof OldIR::PhiInstruction
    } or
    Phi(OldBlock block, Alias::MemoryLocation defLocation) {
      definitionHasPhiNode(defLocation, block)
    } or
    Chi(OldInstruction oldInstruction) {
      not oldInstruction instanceof OldIR::PhiInstruction and
      hasChiNode(_, oldInstruction)
    } or
    Unreached(Function function) {
      exists(OldInstruction oldInstruction |
        function = oldInstruction.getEnclosingFunction() and
        Reachability::isInfeasibleInstructionSuccessor(oldInstruction, _)
      )
    }

  cached predicate hasTempVariable(Function func, Locatable ast, TempVariableTag tag,
      Type type) {
    exists(OldIR::IRTempVariable var |
      var.getEnclosingFunction() = func and
      var.getAST() = ast and
      var.getTag() = tag and
      var.getType() = type
    )
  }

  cached predicate hasModeledMemoryResult(Instruction instruction) {
    exists(Alias::getResultMemoryLocation(getOldInstruction(instruction))) or
    instruction instanceof PhiInstruction or  // Phis always have modeled results
    instruction instanceof ChiInstruction  // Chis always have modeled results
  }

  cached Instruction getRegisterOperandDefinition(Instruction instruction, RegisterOperandTag tag) {
    exists(OldInstruction oldInstruction, OldIR::RegisterOperand oldOperand |
      oldInstruction = getOldInstruction(instruction) and
      oldOperand = oldInstruction.getAnOperand() and
      tag = oldOperand.getOperandTag() and
      result = getNewInstruction(oldOperand.getDefinitionInstruction())
    )
  }

  cached Instruction getMemoryOperandDefinition(Instruction instruction, MemoryOperandTag tag, Overlap overlap) {
    exists(OldInstruction oldInstruction, OldIR::NonPhiMemoryOperand oldOperand |
      oldInstruction = getOldInstruction(instruction) and
      oldOperand = oldInstruction.getAnOperand() and
      tag = oldOperand.getOperandTag() and
      (
        (
          if exists(Alias::getOperandMemoryLocation(oldOperand)) then (
            exists(OldBlock useBlock, int useRank, Alias::MemoryLocation useLocation, Alias::MemoryLocation defLocation,
                OldBlock defBlock, int defRank, int defIndex |
              useLocation = Alias::getOperandMemoryLocation(oldOperand) and
              hasDefinitionAtRank(useLocation, defLocation, defBlock, defRank, defIndex) and
              hasUseAtRank(useLocation, useBlock, useRank, oldInstruction) and
              definitionReachesUse(useLocation, defBlock, defRank, useBlock, useRank) and
              overlap = Alias::getOverlap(defLocation, useLocation) and
              result = getDefinitionOrChiInstruction(defBlock, defIndex, defLocation, overlap)
            )
          )
          else (
            result = instruction.getEnclosingFunctionIR().getUnmodeledDefinitionInstruction() and
            overlap instanceof MustTotallyOverlap
          )
        ) or
        // Connect any definitions that are not being modeled in SSA to the
        // `UnmodeledUse` instruction.
        exists(OldInstruction oldDefinition |
          instruction instanceof UnmodeledUseInstruction and
          tag instanceof UnmodeledUseOperandTag and
          oldDefinition = oldOperand.getDefinitionInstruction() and
          not exists(Alias::getResultMemoryLocation(oldDefinition)) and
          result = getNewInstruction(oldDefinition) and
          overlap instanceof MustTotallyOverlap
        )
      )
    ) or
    instruction = Chi(getOldInstruction(result)) and
    tag instanceof ChiPartialOperandTag and
    overlap instanceof MustExactlyOverlap
    or
    exists(FunctionIR f |
      tag instanceof UnmodeledUseOperandTag and
      result = f.getUnmodeledDefinitionInstruction() and
      instruction = f.getUnmodeledUseInstruction() and
      overlap instanceof MustTotallyOverlap
    )
    or
    tag instanceof ChiTotalOperandTag and
    result = getChiInstructionTotalOperand(instruction) and
    overlap instanceof MustExactlyOverlap
  }

  cached Type getInstructionOperandType(Instruction instr, TypedOperandTag tag) {
    exists(OldInstruction oldInstruction, OldIR::TypedOperand oldOperand |
      oldInstruction = getOldInstruction(instr) and
      oldOperand = oldInstruction.getAnOperand() and
      tag = oldOperand.getOperandTag() and
      result = oldOperand.getType()
    )
  }

  cached int getInstructionOperandSize(Instruction instr, SideEffectOperandTag tag) {
    exists(OldInstruction oldInstruction, OldIR::SideEffectOperand oldOperand |
      oldInstruction = getOldInstruction(instr) and
      oldOperand = oldInstruction.getAnOperand() and
      tag = oldOperand.getOperandTag() and
      // Only return a result for operands that need an explicit result size.
      oldOperand.getType() instanceof UnknownType and
      result = oldOperand.getSize()
    )
  }

  cached Instruction getPhiOperandDefinition(PhiInstruction instr,
      IRBlock newPredecessorBlock, Overlap overlap) {
    exists(Alias::MemoryLocation useLocation, Alias::MemoryLocation defLocation, OldBlock phiBlock,
        OldBlock defBlock, int defRank, int defIndex, OldBlock predBlock |
      definitionHasPhiNode(useLocation, phiBlock) and
      predBlock = phiBlock.getAFeasiblePredecessor() and
      instr = Phi(phiBlock, useLocation) and
      newPredecessorBlock = getNewBlock(predBlock) and
      hasDefinitionAtRank(useLocation, defLocation, defBlock, defRank, defIndex) and
      definitionReachesEndOfBlock(useLocation, defBlock, defRank, predBlock) and
      overlap = Alias::getOverlap(defLocation, useLocation) and
      result = getDefinitionOrChiInstruction(defBlock, defIndex, defLocation, overlap)
    )
  }

  cached Instruction getChiInstructionTotalOperand(ChiInstruction chiInstr) {
    exists(Alias::VirtualVariable vvar, OldInstruction oldInstr, OldBlock defBlock,
        int defRank, int defIndex, OldBlock useBlock, int useRank |
      chiInstr = Chi(oldInstr) and
      vvar = Alias::getResultMemoryLocation(oldInstr).getVirtualVariable() and
      hasDefinitionAtRank(vvar, _, defBlock, defRank, defIndex) and
      hasUseAtRank(vvar, useBlock, useRank, oldInstr) and
      definitionReachesUse(vvar, defBlock, defRank, useBlock, useRank) and
      if defIndex >= 0 then
        result = getNewFinalInstruction(defBlock.getInstruction(defIndex))
      else
        result = Phi(defBlock, vvar)
    )
  }

  cached Instruction getPhiInstructionBlockStart(PhiInstruction instr) {
    exists(OldBlock oldBlock |
      instr = Phi(oldBlock, _) and
      result = getNewInstruction(oldBlock.getFirstInstruction())
    )
  }

  cached Expr getInstructionConvertedResultExpression(Instruction instruction) {
    result = getOldInstruction(instruction).getConvertedResultExpression()
  }

  cached Expr getInstructionUnconvertedResultExpression(Instruction instruction) {
    result = getOldInstruction(instruction).getUnconvertedResultExpression()
  }

  /*
   * This adds Chi nodes to the instruction successor relation; if an instruction has a Chi node,
   * that node is its successor in the new successor relation, and the Chi node's successors are
   * the new instructions generated from the successors of the old instruction
   */
  cached Instruction getInstructionSuccessor(Instruction instruction, EdgeKind kind) {
    if(hasChiNode(_, getOldInstruction(instruction)))
    then
      result = Chi(getOldInstruction(instruction)) and
      kind instanceof GotoEdge
    else (
      exists(OldInstruction oldInstruction |
        oldInstruction = getOldInstruction(instruction) and
        (
          if Reachability::isInfeasibleInstructionSuccessor(oldInstruction, kind) then (
            result = Unreached(instruction.getEnclosingFunction())
          )
          else (
            result = getNewInstruction(oldInstruction.getSuccessor(kind))
          )
        )
      ) or
      exists(OldInstruction oldInstruction |
        instruction = Chi(oldInstruction) and
        result = getNewInstruction(oldInstruction.getSuccessor(kind))
      )
    )
  }

  cached Instruction getInstructionBackEdgeSuccessor(Instruction instruction, EdgeKind kind) {
    exists(OldInstruction oldInstruction |
      not Reachability::isInfeasibleInstructionSuccessor(oldInstruction, kind) and
      // There is only one case for the translation into `result` because the
      // SSA construction never inserts extra instructions _before_ an existing
      // instruction.
      getOldInstruction(result) = oldInstruction.getBackEdgeSuccessor(kind) and
      // There are two cases for the translation into `instruction` because the
      // SSA construction might have inserted a chi node _after_
      // `oldInstruction`, in which case the back edge should come out of the
      // chi node instead.
      if hasChiNode(_, oldInstruction)
      then instruction = Chi(oldInstruction)
      else instruction = getNewInstruction(oldInstruction)
    )
  }

  cached Locatable getInstructionAST(Instruction instruction) {
    exists(OldInstruction oldInstruction |
      instruction = WrappedInstruction(oldInstruction)
      or
      instruction = Chi(oldInstruction)
    |
      result = oldInstruction.getAST()
    )
    or
    exists(OldBlock block |
      instruction = Phi(block, _) and
      result = block.getFirstInstruction().getAST()
    )
    or
    instruction = Unreached(result)
  }

  cached predicate instructionHasType(Instruction instruction, Type type, boolean isGLValue) {
    exists(OldInstruction oldInstruction |
      instruction = WrappedInstruction(oldInstruction) and
      type = oldInstruction.getResultType() and
      if oldInstruction.isGLValue()
      then isGLValue = true
      else isGLValue = false
    )
    or
    exists(OldInstruction oldInstruction, Alias::VirtualVariable vvar |
      instruction = Chi(oldInstruction) and
      hasChiNode(vvar, oldInstruction) and
      type = vvar.getType() and
      isGLValue = false
    )
    or
    exists(Alias::MemoryLocation location |
      instruction = Phi(_, location) and
      type = location.getType() and
      isGLValue = false
    )
    or
    instruction = Unreached(_) and
    type instanceof VoidType and
    isGLValue = false
  }

  cached Opcode getInstructionOpcode(Instruction instruction) {
    exists(OldInstruction oldInstruction |
      instruction = WrappedInstruction(oldInstruction) and
      result = oldInstruction.getOpcode()
    )
    or
    instruction instanceof Chi and
    result instanceof Opcode::Chi
    or
    instruction instanceof Phi and
    result instanceof Opcode::Phi
    or
    instruction instanceof Unreached and
    result instanceof Opcode::Unreached
  }

  cached FunctionIR getInstructionEnclosingFunctionIR(Instruction instruction) {
    exists(OldInstruction oldInstruction |
      instruction = WrappedInstruction(oldInstruction)
      or
      instruction = Chi(oldInstruction)
    |
      result.getFunction() = oldInstruction.getEnclosingFunction()
    )
    or
    exists(OldBlock block |
      instruction = Phi(block, _) and
      result.getFunction() = block.getEnclosingFunction()
    )
    or
    instruction = Unreached(result.getFunction())
  }

  cached IRVariable getInstructionVariable(Instruction instruction) {
    result = getNewIRVariable(getOldInstruction(instruction).(OldIR::VariableInstruction).getVariable())
  }

  cached Field getInstructionField(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::FieldInstruction).getField()
  }

  cached Function getInstructionFunction(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::FunctionInstruction).getFunctionSymbol()
  }

  cached string getInstructionConstantValue(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::ConstantValueInstruction).getValue()
  }

  cached StringLiteral getInstructionStringLiteral(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::StringConstantInstruction).getValue()
  }

  cached Type getInstructionExceptionType(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::CatchByTypeInstruction).getExceptionType()
  }

  cached int getInstructionElementSize(Instruction instruction) {
    result = getOldInstruction(instruction).(OldIR::PointerArithmeticInstruction).getElementSize()
  }

  cached int getInstructionResultSize(Instruction instruction) {
    // Only return a result for instructions that needed an explicit result size.
    instruction.getResultType() instanceof UnknownType and
    result = getOldInstruction(instruction).getResultSize()
  }

  cached predicate getInstructionInheritance(Instruction instruction, Class baseClass,
      Class derivedClass) {
    exists(OldIR::InheritanceConversionInstruction oldInstr |
      oldInstr = getOldInstruction(instruction) and
      baseClass = oldInstr.getBaseClass() and
      derivedClass = oldInstr.getDerivedClass()
    )
  }

  cached Instruction getPrimaryInstructionForSideEffect(Instruction instruction) {
    exists(OldIR::SideEffectInstruction oldInstruction |
      oldInstruction = getOldInstruction(instruction) and
      result = getNewInstruction(oldInstruction.getPrimaryInstruction())
    )
    or
    exists(OldIR::Instruction oldInstruction |
      instruction = Chi(oldInstruction) and
      result = getNewInstruction(oldInstruction)
    )
  }
}

private predicate hasChiNode(Alias::VirtualVariable vvar, OldInstruction def) {
  exists(Alias::MemoryLocation defLocation |
    defLocation = Alias::getResultMemoryLocation(def) and
    defLocation.getVirtualVariable() = vvar and
    defLocation != vvar  // ??? How to handle total definitions that aren't the virtual variable itself?
  )
}

/**
  * Gets the rank index of a hyphothetical use one instruction past the end of
  * the block. This index can be used to determine if a definition reaches the
  * end of the block, even if the definition is the last instruction in the
  * block.
  */
private int exitRank(Alias::MemoryLocation useLocation, OldBlock block) {
  result = max(int rankIndex | defUseRank(useLocation, block, rankIndex, _)) + 1
}

/**
  * Holds if a definition that overlaps `useLocation` at (`defBlock`, `defRank`) reaches the use of `useLocation` at
  * (`useBlock`, `useRank`) without any intervening definitions that overlap `useLocation`, where `defBlock` and
  * `useBlock` are the same block.
  */
private predicate definitionReachesUseWithinBlock(Alias::MemoryLocation useLocation, OldBlock defBlock,
    int defRank, OldBlock useBlock, int useRank) {
  defBlock = useBlock and
  hasDefinitionAtRank(useLocation, _, defBlock, defRank, _) and
  hasUseAtRank(useLocation, useBlock, useRank, _) and
  definitionReachesRank(useLocation, defBlock, defRank, useRank)
}

/**
  * Holds if a definition that overlaps `useLocation` at (`defBlock`, `defRank`) reaches the use of `useLocation` at
  * (`useBlock`, `useRank`) without any intervening definitions that overlap `useLocation`.
  */
predicate definitionReachesUse(Alias::MemoryLocation useLocation, OldBlock defBlock,
    int defRank, OldBlock useBlock, int useRank) {
  hasUseAtRank(useLocation, useBlock, useRank, _) and
  (
    definitionReachesUseWithinBlock(useLocation, defBlock, defRank, useBlock,
      useRank) or
    (
      definitionReachesEndOfBlock(useLocation, defBlock, defRank,
        useBlock.getAFeasiblePredecessor()) and
      not definitionReachesUseWithinBlock(useLocation, useBlock, _, useBlock, useRank)
    )
  )
}

/**
  * Gets the set of memory accesses that, when used as a definition, may overlap the memory access specified by
  * `useLocation`.
  */
private Alias::MemoryLocation getDefiningLocations(Alias::MemoryLocation useLocation) {
  exists(Alias::getOverlap(result, useLocation))
}

/**
  * Holds if the definition that overlaps `useLocation` at `(block, defRank)` reaches the rank
  * index `reachesRank` in block `block`.
  */
private predicate definitionReachesRank(Alias::MemoryLocation useLocation, OldBlock block, int defRank,
    int reachesRank) {
  hasDefinitionAtRank(useLocation, _, block, defRank, _) and
  reachesRank <= exitRank(useLocation, block) and  // Without this, the predicate would be infinite.
  (
    // The def always reaches the next use, even if there is also a def on the
    // use instruction.
    reachesRank = defRank + 1 or
    (
      // If the def reached the previous rank, it also reaches the current rank,
      // unless there was another def at the previous rank.
      definitionReachesRank(useLocation, block, defRank, reachesRank - 1) and
      not hasDefinitionAtRank(useLocation, _, block, reachesRank - 1, _)
    )
  )
}

/**
  * Holds if the definition that overlaps `useLocation` at `(defBlock, defRank)` reaches the end of
  * block `block` without any intervening definitions that overlap `useLocation`.
  */
private predicate definitionReachesEndOfBlock(Alias::MemoryLocation useLocation, OldBlock defBlock,
    int defRank, OldBlock block) {
  hasDefinitionAtRank(useLocation, _, defBlock, defRank, _) and
  (
    (
      // If we're looking at the def's own block, just see if it reaches the exit
      // rank of the block.
      block = defBlock and
      locationLiveOnExitFromBlock(useLocation, defBlock) and
      definitionReachesRank(useLocation, defBlock, defRank, exitRank(useLocation, defBlock))
    ) or
    exists(OldBlock idom |
      definitionReachesEndOfBlock(useLocation, defBlock, defRank, idom) and
      noDefinitionsSinceIDominator(useLocation, idom, block)
    )
  )
}

pragma[noinline]
private predicate noDefinitionsSinceIDominator(Alias::MemoryLocation useLocation, OldBlock idom,
    OldBlock block) {
  Dominance::blockImmediatelyDominates(idom, block) and // It is sufficient to traverse the dominator graph, cf. discussion above.
  locationLiveOnExitFromBlock(useLocation, block) and
  not hasDefinition(useLocation, _, block, _)
}

private predicate locationLiveOnEntryToBlock(Alias::MemoryLocation useLocation, OldBlock block) {
  exists(int firstAccess |
    hasUse(useLocation, block, firstAccess, _) and
    firstAccess = min(int index |
      hasUse(useLocation, block, index, _)
      or
      hasNonPhiDefinition(useLocation, _, block, index)
    )
  )
  or
  (locationLiveOnExitFromBlock(useLocation, block) and not hasNonPhiDefinition(useLocation, _, block, _))
}

pragma[noinline]
private predicate locationLiveOnExitFromBlock(Alias::MemoryLocation useLocation, OldBlock block) {
  locationLiveOnEntryToBlock(useLocation, block.getAFeasibleSuccessor())
}

private predicate definitionHasUse(Alias::MemoryLocation defLocation, OldBlock block, int index) {
  exists(OldInstruction use, Alias::MemoryLocation useLocation |
    block.getInstruction(index) = use and
    useLocation = Alias::getOperandMemoryLocation(use.getAnOperand()) and
    if defLocation instanceof Alias::VirtualVariable then (
      // For a virtual variable, any use of a location that is a member of the virtual variable counts as a use.
      defLocation = useLocation.getVirtualVariable() or
      exists(Alias::MemoryLocation partialDefLocation |
        partialDefLocation = Alias::getResultMemoryLocation(use) and
        /*
          * a partial write to a virtual variable is going to generate a use of that variable when
          * Chi nodes are inserted, so we need to mark it as a use in the old IR
          */
        partialDefLocation.getVirtualVariable() = defLocation and
        partialDefLocation != defLocation // ??? Should this be just a check for a partial write, to handle an exact overlap with a different type?
      )
    )
    else (
      // For other locations, only an exactly-overlapping use of the same location counts as a use.
      defLocation = useLocation and
      Alias::getOverlap(defLocation, useLocation) instanceof MustExactlyOverlap
    )
  )
}

private predicate definitionHasRedefinition(Alias::MemoryLocation defLocation, OldBlock block, int index) {
  exists(OldInstruction redef, Alias::MemoryLocation redefLocation |
    block.getInstruction(index) = redef and
    redefLocation = Alias::getResultMemoryLocation(redef) and
    if defLocation instanceof Alias::VirtualVariable then (
      // For a virtual variable, the definition may be consumed by any use of a location that is a member of the
      // virtual variable. Thus, the definition is live until a subsequent redefinition of the entire virtual
      // variable.
      redefLocation = defLocation  // ??? Should probably include all total definitions of the virtual variable
    )
    else (
      // For other locations, the definition may only be consumed by an exactly-overlapping use of the same location.
      // Thus, the definition is live until a subsequent definition of any location that may overlap the original
      // definition location.
      redefLocation = getDefiningLocations(defLocation)
    )
  )
}

/**
  * Holds if the definition `defLocation` is live on entry to block `block`. The definition is live if there is at
  * least one use of that definition before any intervening instruction that redefines the definition location.
  */
private predicate definitionLiveOnEntryToBlock(Alias::MemoryLocation defLocation, OldBlock block) {
  exists(int firstAccess |
    definitionHasUse(defLocation, block, firstAccess) and
    firstAccess = min(int index |
        definitionHasUse(defLocation, block, index)
        or
        definitionHasRedefinition(defLocation, block, index)
      )
  )
  or
  (definitionLiveOnExitFromBlock(defLocation, block) and not definitionHasRedefinition(defLocation, block, _))
}

pragma[noinline]
private predicate definitionLiveOnExitFromBlock(Alias::MemoryLocation defLocation, OldBlock block) {
  definitionLiveOnEntryToBlock(defLocation, block.getAFeasibleSuccessor())
}

/**
  * Holds if the virtual variable `vvar` has a definition in block `block`, either because of an existing instruction
  * or because of a Phi node.
  */
private predicate definitionHasDefinitionInBlock(Alias::MemoryLocation defLocation, OldBlock block) {
  definitionHasPhiNode(defLocation, block) or
  exists(OldInstruction def |
    defLocation = Alias::getResultMemoryLocation(def) and
    def.getBlock() = block
  )
}

private predicate definitionHasPhiNode(Alias::MemoryLocation defLocation, OldBlock phiBlock) {
  exists(OldBlock defBlock |
    phiBlock = Dominance::getDominanceFrontier(defBlock) and
    definitionHasDefinitionInBlock(defLocation, defBlock) and
    /* We can also eliminate those nodes where the definition is not live on any incoming edge */
    definitionLiveOnEntryToBlock(defLocation, phiBlock)
  )
}

/** 
  * Holds if there is a definition at index `index` in block `block` that overlaps memory location `useLocation`.
  * This predicate does not include definitions for Phi nodes.
  */
private predicate hasNonPhiDefinition(Alias::MemoryLocation useLocation, Alias::MemoryLocation defLocation,
    OldBlock block, int index) {
  exists(OldInstruction def |
    defLocation = Alias::getResultMemoryLocation(def) and
    block.getInstruction(index) = def and
    defLocation = getDefiningLocations(useLocation)
  )
}

/** 
  * Holds if there is a definition at index `index` in block `block` that overlaps memory location `useLocation`.
  * This predicate includes definitions for Phi nodes (at index -2 for virtual variables, or index -1 for precise
  * locations).
  */
private predicate hasDefinition(Alias::MemoryLocation useLocation, Alias::MemoryLocation defLocation, OldBlock block,
    int index) {
  (
    // If there is a Phi node for the use location itself, treat that as a definition at index -1.
    not useLocation instanceof Alias::VirtualVariable and
    definitionHasPhiNode(useLocation, block) and
    index = -1
  ) or
  (
    // If there is a Phi node for the virtual variable of the use location, treat that as a definition at index -2,
    // which makes it occur before any more-specific Phi node for the exact use location.
    definitionHasPhiNode(useLocation.getVirtualVariable(), block) and
    index = -2
  ) or
  hasNonPhiDefinition(useLocation, defLocation, block, index)
}

/**
  * Holds if there is a definition at index `index` in block `block` that overlaps memory location `useLocation`.
  * `rankIndex` is the rank of the definition as computed by `defUseRank()`.
  */
predicate hasDefinitionAtRank(Alias::MemoryLocation useLocation, Alias::MemoryLocation defLocation,
    OldBlock block, int rankIndex, int instructionIndex) {
  hasDefinition(useLocation, defLocation, block, instructionIndex) and
  defUseRank(useLocation, block, rankIndex, instructionIndex)
}

/**
  * Holds if there is a use of `useLocation` on instruction `use` at index `index` in block `block`.
  */
private predicate hasUse(Alias::MemoryLocation useLocation, OldBlock block, int index,
    OldInstruction use) {
  block.getInstruction(index) = use and
  (
    useLocation = Alias::getOperandMemoryLocation(use.getAnOperand()) or
    exists(Alias::MemoryLocation defLocation |
      defLocation = Alias::getResultMemoryLocation(use) and
      /*
        * a partial write to a virtual variable is going to generate a use of that variable when
        * Chi nodes are inserted, so we need to mark it as a use in the old IR
        */
      defLocation.getVirtualVariable() = useLocation and
      defLocation != useLocation  // ??? Should this be just a check for a partial write, to handle an exact overlap with a different type?
    )
  )
}

/**
  * Holds if there is a use of memory location `useLocation` on instruction `use` in block `block`. `rankIndex` is the
  * rank of the use use as computed by `defUseRank`.
  */
predicate hasUseAtRank(Alias::MemoryLocation useLocation, OldBlock block, int rankIndex, OldInstruction use) {
  exists(int index |
    hasUse(useLocation, block, index, use) and
    defUseRank(useLocation, block, rankIndex, index)
  )
}

/**
  * Holds if there is a definition at index `index` in block `block` that overlaps memory location `useLocation`, or
  * a use of `useLocation` at index `index` in block `block`. `rankIndex` is the sequence number of the definition or
  * use within `block`, counting only uses of `useLocation` and definitions that overlap `useLocation`.
  */
private predicate defUseRank(Alias::MemoryLocation useLocation, OldBlock block, int rankIndex, int index) {
  index = rank[rankIndex](int j | hasDefinition(useLocation, _, block, j) or hasUse(useLocation, block, j, _))
}

predicate getDebugChiInstructionTotalOperand(OldInstruction oldInstr, OldBlock defBlock, int defIndex) {
  exists(Alias::VirtualVariable vvar, int defRank, OldBlock useBlock, int useRank |
    hasChiNode(_, oldInstr) and
    vvar = Alias::getResultMemoryLocation(oldInstr).getVirtualVariable() and
    hasDefinitionAtRank(vvar, _, defBlock, defRank, defIndex) and
    hasUseAtRank(vvar, useBlock, useRank, oldInstr) and
    definitionReachesUse(vvar, defBlock, defRank, useBlock, useRank)
  )
}

import CachedForDebugging
cached private module CachedForDebugging {
  cached string getTempVariableUniqueId(IRTempVariable var) {
    result = getOldTempVariable(var).getUniqueId()
  }

  cached string getInstructionUniqueId(Instruction instr) {
    exists(OldInstruction oldInstr |
      oldInstr = getOldInstruction(instr) and
      result = "NonSSA: " + oldInstr.getUniqueId()
    ) or
    exists(Alias::MemoryLocation location, OldBlock phiBlock |
      instr = Phi(phiBlock, location) and
      result = "Phi Block(" + phiBlock.getUniqueId() + "): " + location.getUniqueId() 
    ) or
    (
      instr = Unreached(_) and
      result = "Unreached"
    )
  }

  private OldIR::IRTempVariable getOldTempVariable(IRTempVariable var) {
    result.getEnclosingFunction() = var.getEnclosingFunction() and
    result.getAST() = var.getAST() and
    result.getTag() = var.getTag()
  }
}
