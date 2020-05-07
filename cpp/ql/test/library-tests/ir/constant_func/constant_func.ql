import default
import semmle.code.cpp.ir.IR
import semmle.code.cpp.ir.implementation.aliased_ssa.constant.ConstantAnalysis
import semmle.code.cpp.ir.internal.IntegerConstant

from IRFunction irFunc, int value
where
  value =
    getValue(getConstantValue(irFunc
            .getReturnInstruction()
            .(ReturnValueInstruction)
            .getReturnValue()))
select irFunc, value

/**
 * Original query as a query predicate.
 */
query predicate debug_01(IRFunction irFunc, int value) {
  value =
    getValue(getConstantValue(irFunc
            .getReturnInstruction()
            .(ReturnValueInstruction)
            .getReturnValue()))
}

/**
 * Add context to narrow results to a single function.
 */
query predicate debug_02(IRFunction irFunc, int value) {
  irFunc.toString() = "IR: ReturnDifference" and
  value =
    getValue(getConstantValue(irFunc
            .getReturnInstruction()
            .(ReturnValueInstruction)
            .getReturnValue()))
}

/**
 * Drill into `getConstantValue()` subexpression.
 */
query predicate debug_03(IRFunction irFunc, int r) {
  irFunc.toString() = "IR: ReturnDifference" and
  r = getConstantValue(irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue())
}

/**
 * Step into `getConstantValue()` call, pushing previous context
 */
language[monotonicAggregates]
query int debug_04(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      result = instr.(IntegerConstantInstruction).getValue().toInt()
      or
      result = getBinaryInstructionValue(instr)
      or
      result = neg(getConstantValue(instr.(NegateInstruction).getUnary()))
      or
      result = getConstantValue(instr.(CopyInstruction).getSourceValue())
      or
      exists(PhiInstruction phi |
        phi = instr and
        result = unique(Operand op | op = phi.getAnInputOperand() | getConstantValue(op.getDef()))
      )
    )
  )
}

/**
 * Highlight each term of disjunction.
 */
language[monotonicAggregates]
query int debug_05_01(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      result = instr.(IntegerConstantInstruction).getValue().toInt()
    )
  )
}

language[monotonicAggregates]
query int debug_05_02(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      result = getBinaryInstructionValue(instr)
    )
  )
}

language[monotonicAggregates]
query int debug_05_03(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      result = neg(getConstantValue(instr.(NegateInstruction).getUnary()))
    )
  )
}

language[monotonicAggregates]
query int debug_05_04(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      result = getConstantValue(instr.(CopyInstruction).getSourceValue())
    )
  )
}

language[monotonicAggregates]
query int debug_05_05(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    instr = ctx1_instr and  // Apply context
    (
      exists(PhiInstruction phi |
        phi = instr and
        result = unique(Operand op | op = phi.getAnInputOperand() | getConstantValue(op.getDef()))
      )
    )
  )
}

/**
 * Step into another `getConstantValue()` call.
 */
language[monotonicAggregates]
query int debug_06(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    exists(Instruction ctx2_instr |
      ctx2_instr = ctx1_instr.(CopyInstruction).getSourceValue() and
      instr = ctx2_instr and  // Apply context
      (
        result = instr.(IntegerConstantInstruction).getValue().toInt()
        or
        result = getBinaryInstructionValue(instr)
        or
        result = neg(getConstantValue(instr.(NegateInstruction).getUnary()))
        or
        result = getConstantValue(instr.(CopyInstruction).getSourceValue())
        or
        exists(PhiInstruction phi |
          phi = instr and
          result = unique(Operand op | op = phi.getAnInputOperand() | getConstantValue(op.getDef()))
        )
      )
    )
  )
}

/**
 * Drill into one branch of the disjunction.
 */
language[monotonicAggregates]
query int debug_07(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    exists(Instruction ctx2_instr |
      ctx2_instr = ctx1_instr.(CopyInstruction).getSourceValue() and
      instr = ctx2_instr and  // Apply context
      (
        result = getBinaryInstructionValue(instr)
      )
    )
  )
}

/**
 * Step into the `getBinaryInstructionValue()` call.
 */
language[monotonicAggregates]
query int debug_08(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    exists(Instruction ctx2_instr |
      ctx2_instr = ctx1_instr.(CopyInstruction).getSourceValue() and
      exists(Instruction ctx3_instr |
        ctx3_instr = ctx2_instr and
        instr = ctx3_instr and  // Apply context
        (
          exists(int left, int right |
            binaryInstructionOperands(instr, left, right) and
            (
              instr instanceof AddInstruction and result = add(left, right)
              or
              instr instanceof SubInstruction and result = add(left, right)
              or
              instr instanceof MulInstruction and result = mul(left, right)
              or
              instr instanceof DivInstruction and result = div(left, right)
              or
              instr instanceof CompareEQInstruction and result = compareEQ(left, right)
              or
              instr instanceof CompareNEInstruction and result = compareNE(left, right)
              or
              instr instanceof CompareLTInstruction and result = compareLT(left, right)
              or
              instr instanceof CompareGTInstruction and result = compareGT(left, right)
              or
              instr instanceof CompareLEInstruction and result = compareLE(left, right)
              or
              instr instanceof CompareGEInstruction and result = compareGE(left, right)
            )
          )
        )
      )
    )
  )
}

/**
 * Drill into one branch of the disjunction, applying context from the other term of the conjunction.
 */
language[monotonicAggregates]
query int debug_09(Instruction instr) {
  exists(IRFunction ctx1_irFunc, Instruction ctx1_instr |
    ctx1_irFunc.toString() = "IR: ReturnDifference" and  // Explicit context
    ctx1_instr = ctx1_irFunc.getReturnInstruction().(ReturnValueInstruction).getReturnValue() and // Implicit context from call site
    exists(Instruction ctx2_instr |
      ctx2_instr = ctx1_instr.(CopyInstruction).getSourceValue() and
      exists(Instruction ctx3_instr |
        ctx3_instr = ctx2_instr and
        instr = ctx3_instr and  // Apply context
        (
          exists(int left, int right |
            binaryInstructionOperands(instr, left, right) and
            (
              instr instanceof SubInstruction and result = add(left, right)
            )
          )
        )
      )
    )
  )
}

