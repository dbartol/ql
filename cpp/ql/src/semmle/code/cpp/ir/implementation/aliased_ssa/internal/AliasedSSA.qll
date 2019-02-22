import cpp
import AliasAnalysis
import semmle.code.cpp.ir.internal.Overlap
private import semmle.code.cpp.Print
private import semmle.code.cpp.ir.implementation.unaliased_ssa.IR
private import semmle.code.cpp.ir.internal.IntegerConstant as Ints
private import semmle.code.cpp.ir.internal.IntegerInterval as Interval
private import semmle.code.cpp.ir.internal.OperandTag

private class IntValue = Ints::IntValue;

private predicate hasResultMemoryAccess(Instruction instr, IRVariable var, Type type, IntValue startBitOffset,
    IntValue endBitOffset) {
  resultPointsTo(instr.getResultAddressOperand().getDefinitionInstruction(), var, startBitOffset) and
  type = instr.getResultType() and
  if exists(instr.getResultSize()) then
    endBitOffset = Ints::add(startBitOffset, Ints::mul(instr.getResultSize(), 8))
  else
    endBitOffset = Ints::unknown()
}

private predicate hasOperandMemoryAccess(MemoryOperand operand, IRVariable var, Type type, IntValue startBitOffset,
    IntValue endBitOffset) {
  resultPointsTo(operand.getAddressOperand().getDefinitionInstruction(), var, startBitOffset) and
  type = operand.getType() and
  if exists(operand.getSize()) then
    endBitOffset = Ints::add(startBitOffset, Ints::mul(operand.getSize(), 8))
  else
    endBitOffset = Ints::unknown()
}

private newtype TMemoryLocation =
  TVariableMemoryLocation(IRVariable var, Type type, IntValue startBitOffset, IntValue endBitOffset) {
    hasResultMemoryAccess(_, var, type, startBitOffset, endBitOffset) or
    hasOperandMemoryAccess(_, var, type, startBitOffset, endBitOffset)
  }
  or
  TUnknownMemoryLocation(FunctionIR funcIR) or
  TUnknownVirtualVariable(FunctionIR funcIR)

abstract class MemoryLocation extends TMemoryLocation {
  abstract string toString();
  
  abstract VirtualVariable getVirtualVariable();

  abstract Type getType();

  abstract string getUniqueId();
}

abstract class VirtualVariable extends MemoryLocation {
}

/**
 * An access to memory within a single known `IRVariable`. The variable may be either an unescaped variable
 * (with its own `VirtualIRVariable`) or an escaped variable (assiged to `UnknownVirtualVariable`).
 */
class VariableMemoryLocation extends TVariableMemoryLocation, MemoryLocation {
  IRVariable var;
  Type type;
  IntValue startBitOffset;
  IntValue endBitOffset;

  VariableMemoryLocation() {
    this = TVariableMemoryLocation(var, type, startBitOffset, endBitOffset)
  }

  override final string toString() {
    result = var.toString() + Interval::getIntervalString(startBitOffset, endBitOffset) + "<" + type.toString() + ">"
  }

  override final Type getType() {
    result = type
  }

  final IntValue getStartBitOffset() {
    result = startBitOffset
  }
  
  final IntValue getEndBitOffset() {
    result = endBitOffset
  }
  
  final IRVariable getVariable() {
    result = var
  }

  override final string getUniqueId() {
    result = var.getUniqueId() + Interval::getIntervalString(startBitOffset, endBitOffset) + "<" +
      getTypeIdentityString(type) + ">"
  }

  override final VirtualVariable getVirtualVariable() {
    if variableAddressEscapes(var) then
      result = TUnknownVirtualVariable(var.getEnclosingFunctionIR())
    else
      result = TVariableMemoryLocation(var, var.getType(), 0, var.getType().getSize() * 8)
  }
}

class VariableVirtualVariable extends VariableMemoryLocation, VirtualVariable {
  VariableVirtualVariable() {
    not variableAddressEscapes(var) and
    type = var.getType() and
    startBitOffset = 0 and
    Ints::isEQ(endBitOffset, Ints::mul(var.getType().getSize(), 8))
  }
}

/**
 * An access to memory that is not known to be confined to a specific `IRVariable`.
 */
class UnknownMemoryLocation extends TUnknownMemoryLocation, MemoryLocation {
  FunctionIR funcIR;

  UnknownMemoryLocation() {
    this = TUnknownMemoryLocation(funcIR)
  }
  
  override final string toString() {
    result = "{Unknown}"
  }
  
  override final VirtualVariable getVirtualVariable() {
    result = TUnknownVirtualVariable(funcIR)
  }

  override final Type getType() {
    result instanceof UnknownType
  }

  override final string getUniqueId() {
    result = "{Unknown}"
  }
}

/**
 * An access to all aliased memory.
 */
class UnknownVirtualVariable extends TUnknownVirtualVariable, VirtualVariable {
  FunctionIR funcIR;

  UnknownVirtualVariable() {
    this = TUnknownVirtualVariable(funcIR)
  }
  
  override final string toString() {
    result = "{AllAliased}"
  }

  override final Type getType() {
    result instanceof UnknownType
  }

  override final string getUniqueId() {
    result = " " + toString()
  }

  override final VirtualVariable getVirtualVariable() {
    result = this
  }
}

Overlap getOverlap(MemoryLocation def, MemoryLocation use) {
  def.getVirtualVariable() = use.getVirtualVariable() and
  (
    // An UnknownVirtualVariable must totally overlap any location within the same virtual variable.
    def instanceof UnknownVirtualVariable and result instanceof MustTotallyOverlap or
    // An UnknownMemoryLocation may partially overlap any Location within the same virtual variable.
    def instanceof UnknownMemoryLocation and result instanceof MayPartiallyOverlap or
    exists(VariableMemoryLocation defVariableLocation |
      defVariableLocation = def and
      (
        (
          // A VariableMemoryLocation may partially overlap an unknown location within the same virtual variable.
          ((use instanceof UnknownMemoryLocation) or (use instanceof UnknownVirtualVariable)) and
          result instanceof MayPartiallyOverlap
        ) or
        // A VariableMemoryLocation overlaps another location within the same variable based on the relationship
        // of the two offset intervals.
        exists(VariableMemoryLocation useVariableLocation, IntValue defStartOffset, IntValue defEndOffset,
            IntValue useStartOffset, IntValue useEndOffset |
          useVariableLocation = use and
          defStartOffset = defVariableLocation.getStartBitOffset() and
          defEndOffset = defVariableLocation.getEndBitOffset() and
          useStartOffset = useVariableLocation.getStartBitOffset() and
          useEndOffset = useVariableLocation.getEndBitOffset() and
          result = Interval::getOverlap(defStartOffset, defEndOffset, useStartOffset, useEndOffset)
        )
      )
    )
  )
}

MemoryLocation getResultMemoryLocation(Instruction instr) {
  exists(MemoryAccessKind kind |
    kind = instr.getResultMemoryAccess() and
    (
      (
        kind.usesAddressOperand() and
        if hasResultMemoryAccess(instr, _, _, _, _) then (
          exists(IRVariable var, Type type, IntValue startBitOffset, IntValue endBitOffset |
            hasResultMemoryAccess(instr, var, type, startBitOffset, endBitOffset) and
            result = TVariableMemoryLocation(var, type, startBitOffset, endBitOffset)
          )
        )
        else (
          result = TUnknownMemoryLocation(instr.getEnclosingFunctionIR())
        )
      ) or
      (
        kind instanceof EscapedMemoryAccess and
        result = TUnknownVirtualVariable(instr.getEnclosingFunctionIR())
      ) or
      (
        kind instanceof EscapedMayMemoryAccess and
        result = TUnknownMemoryLocation(instr.getEnclosingFunctionIR())
      )
    )
  )
}

MemoryLocation getOperandMemoryLocation(MemoryOperand operand) {
  exists(MemoryAccessKind kind |
    kind = operand.getMemoryAccess() and
    (
      (
        kind.usesAddressOperand() and
        if hasOperandMemoryAccess(operand, _, _, _, _) then (
          exists(IRVariable var, Type type, IntValue startBitOffset, IntValue endBitOffset |
            hasOperandMemoryAccess(operand, var, type, startBitOffset, endBitOffset) and
            result = TVariableMemoryLocation(var, type, startBitOffset, endBitOffset)
          )
        )
        else (
          result = TUnknownMemoryLocation(operand.getEnclosingFunctionIR())
        )
      ) or
      (
        kind instanceof EscapedMemoryAccess and
        result = TUnknownVirtualVariable(operand.getEnclosingFunctionIR())
      ) or
      (
        kind instanceof EscapedMayMemoryAccess and
        result = TUnknownMemoryLocation(operand.getEnclosingFunctionIR())
      )
    )
  )
}
