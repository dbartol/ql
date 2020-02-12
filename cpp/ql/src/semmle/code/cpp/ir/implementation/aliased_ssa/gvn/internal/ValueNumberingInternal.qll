private import ValueNumberingImports
private import cpp

newtype TValueNumber =
  TVariableAddressValueNumber(IRFunction irFunc, IRVariable var) {
    variableAddressValueNumber(irFunc, var, _)
  } or
  TInitializeParameterValueNumber(IRFunction irFunc, IRVariable var) {
    initializeParameterValueNumber(irFunc, var, _)
  } or
  TInitializeThisValueNumber(IRFunction irFunc) { initializeThisValueNumber(irFunc, _) } or
  TConstantValueNumber(IRFunction irFunc, IRType type, string value) {
    constantValueNumber(irFunc, type, value, _)
  } or
  TStringConstantValueNumber(IRFunction irFunc, IRType type, string value) {
    stringConstantValueNumber(irFunc, type, value, _)
  } or
  TFieldAddressValueNumber(IRFunction irFunc, Field field, TValueNumber objectAddress) {
    fieldAddressValueNumber(irFunc, field, objectAddress, _)
  } or
  TBinaryValueNumber(
    IRFunction irFunc, Opcode opcode, IRType type, TValueNumber leftOperand,
    TValueNumber rightOperand
  ) {
    binaryValueNumber(irFunc, opcode, type, leftOperand, rightOperand, _)
  } or
  TPointerArithmeticValueNumber(
    IRFunction irFunc, Opcode opcode, IRType type, int elementSize, TValueNumber leftOperand,
    TValueNumber rightOperand
  ) {
    pointerArithmeticValueNumber(irFunc, opcode, type, elementSize, leftOperand, rightOperand, _)
  } or
  TUnaryValueNumber(IRFunction irFunc, Opcode opcode, IRType type, TValueNumber operand) {
    unaryValueNumber(irFunc, opcode, type, operand, _)
  } or
  TInheritanceConversionValueNumber(
    IRFunction irFunc, Opcode opcode, Class baseClass, Class derivedClass, TValueNumber operand
  ) {
    inheritanceConversionValueNumber(irFunc, opcode, baseClass, derivedClass, operand, _)
  } or
  TLoadTotalOverlapValueNumber(
    IRFunction irFunc, IRType type, TValueNumber memOperand, TValueNumber operand
  ) {
    loadTotalOverlapValueNumber(irFunc, type, memOperand, operand, _)
  } or
  TUniqueValueNumber(IRFunction irFunc, Instruction instr) { uniqueValueNumber(irFunc, instr) }

/**
 * A `CopyInstruction` whose source operand's value is congruent to the definition of that source
 * operand.
 * For example:
 * ```
 * Point p = { 1, 2 };
 * Point q = p;
 * int a = p.x;
 * ```
 * The use of `p` on line 2 is linked to the definition of `p` on line 1, and is congruent to that
 * definition because it accesses the exact same memory.
 * The use of `p.x` on line 3 is linked to the definition of `p` on line 1 as well, but is not
 * congruent to that definition because `p.x` accesses only a subset of the memory defined by `p`.
 */
class CongruentCopyInstruction extends CopyInstruction {
  CongruentCopyInstruction() {
    this.getSourceValueOperand().getDefinitionOverlap() instanceof MustExactlyOverlap
  }
}

class LoadTotalOverlapInstruction extends LoadInstruction {
  LoadTotalOverlapInstruction() {
    this.getSourceValueOperand().getDefinitionOverlap() instanceof MustTotallyOverlap
  }
}

/**
 * Holds if this library knows how to assign a value number to the specified instruction, other than
 * a `unique` value number that is never shared by multiple instructions.
 */
private predicate numberableInstruction(Instruction instr) {
  instr instanceof VariableAddressInstruction
  or
  instr instanceof InitializeParameterInstruction
  or
  instr instanceof InitializeThisInstruction
  or
  instr instanceof ConstantInstruction
  or
  instr instanceof StringConstantInstruction
  or
  instr instanceof FieldAddressInstruction
  or
  instr instanceof BinaryInstruction
  or
  instr instanceof UnaryInstruction and not instr instanceof CopyInstruction
  or
  instr instanceof PointerArithmeticInstruction
  or
  instr instanceof CongruentCopyInstruction
  or
  instr instanceof LoadTotalOverlapInstruction
}

private predicate variableAddressValueNumber(
  IRFunction irFunc, IRVariable var, VariableAddressInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getIRVariable() = var
}

private predicate initializeParameterValueNumber(
  IRFunction irFunc, IRVariable var, InitializeParameterInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getIRVariable() = var
}

private predicate initializeThisValueNumber(IRFunction irFunc, InitializeThisInstruction instr) {
  instr.getEnclosingIRFunction() = irFunc
}

private predicate constantValueNumber(
  IRFunction irFunc, IRType type, string value, ConstantInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getResultIRType() = type and
  instr.getValue() = value
}

private predicate stringConstantValueNumber(
  IRFunction irFunc, IRType type, string value, StringConstantInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getResultIRType() = type and
  instr.getValue().getValue() = value
}

private predicate fieldAddressValueNumber(
  IRFunction irFunc, Language::Field field, TValueNumber objectAddress,
  FieldAddressInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getField() = field and
  tvalueNumber(instr.getObjectAddress()) = objectAddress
}

private predicate binaryValueNumber(
  IRFunction irFunc, Opcode opcode, IRType type, TValueNumber leftOperand,
  TValueNumber rightOperand, BinaryInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  not instr instanceof PointerArithmeticInstruction and
  instr.getOpcode() = opcode and
  instr.getResultIRType() = type and
  tvalueNumber(instr.getLeft()) = leftOperand and
  tvalueNumber(instr.getRight()) = rightOperand
}

private predicate pointerArithmeticValueNumber(
  IRFunction irFunc, Opcode opcode, IRType type, int elementSize, TValueNumber leftOperand,
  TValueNumber rightOperand, PointerArithmeticInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getOpcode() = opcode and
  instr.getResultIRType() = type and
  instr.getElementSize() = elementSize and
  tvalueNumber(instr.getLeft()) = leftOperand and
  tvalueNumber(instr.getRight()) = rightOperand
}

private predicate unaryValueNumber(
  IRFunction irFunc, Opcode opcode, IRType type, TValueNumber operand, UnaryInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  not instr instanceof InheritanceConversionInstruction and
  not instr instanceof CopyInstruction and
  not instr instanceof FieldAddressInstruction and
  instr.getOpcode() = opcode and
  instr.getResultIRType() = type and
  tvalueNumber(instr.getUnary()) = operand
}

private predicate inheritanceConversionValueNumber(
  IRFunction irFunc, Opcode opcode, Language::Class baseClass, Language::Class derivedClass,
  TValueNumber operand, InheritanceConversionInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getOpcode() = opcode and
  instr.getBaseClass() = baseClass and
  instr.getDerivedClass() = derivedClass and
  tvalueNumber(instr.getUnary()) = operand
}

private predicate loadTotalOverlapValueNumber(
  IRFunction irFunc, IRType type, TValueNumber memOperand, TValueNumber operand,
  LoadTotalOverlapInstruction instr
) {
  instr.getEnclosingIRFunction() = irFunc and
  instr.getResultIRType() = type and
  tvalueNumber(instr.getAnOperand().(MemoryOperand).getAnyDef()) = memOperand and
  tvalueNumberOfOperand(instr.getAnOperand().(AddressOperand)) = operand
}

/**
 * Holds if `instr` should be assigned a unique value number because this library does not know how
 * to determine if two instances of that instruction are equivalent.
 */
private predicate uniqueValueNumber(IRFunction irFunc, Instruction instr) {
  instr.getEnclosingIRFunction() = irFunc and
  not instr.getResultIRType() instanceof IRVoidType and
  not numberableInstruction(instr)
}

/**
 * Gets the value number assigned to `instr`, if any. Returns at most one result.
 */
cached
TValueNumber tvalueNumber(Instruction instr) {
  result = nonUniqueValueNumber(instr)
  or
  exists(IRFunction irFunc |
    uniqueValueNumber(irFunc, instr) and
    result = TUniqueValueNumber(irFunc, instr)
  )
}

/**
 * Gets the value number assigned to the exact definition of `op`, if any.
 * Returns at most one result.
 */
TValueNumber tvalueNumberOfOperand(Operand op) { result = tvalueNumber(op.getDef()) }

/**
 * Gets the value number assigned to `instr`, if any, unless that instruction is assigned a unique
 * value number.
 */
private TValueNumber nonUniqueValueNumber(Instruction instr) {
  exists(IRFunction irFunc |
    irFunc = instr.getEnclosingIRFunction() and
    (
      exists(IRVariable var |
        variableAddressValueNumber(irFunc, var, instr) and
        result = TVariableAddressValueNumber(irFunc, var)
      )
      or
      exists(IRVariable var |
        initializeParameterValueNumber(irFunc, var, instr) and
        result = TInitializeParameterValueNumber(irFunc, var)
      )
      or
      initializeThisValueNumber(irFunc, instr) and
      result = TInitializeThisValueNumber(irFunc)
      or
      exists(IRType type, string value |
        constantValueNumber(irFunc, type, value, instr) and
        result = TConstantValueNumber(irFunc, type, value)
      )
      or
      exists(IRType type, string value |
        stringConstantValueNumber(irFunc, type, value, instr) and
        result = TStringConstantValueNumber(irFunc, type, value)
      )
      or
      exists(Language::Field field, TValueNumber objectAddress |
        fieldAddressValueNumber(irFunc, field, objectAddress, instr) and
        result = TFieldAddressValueNumber(irFunc, field, objectAddress)
      )
      or
      exists(Opcode opcode, IRType type, TValueNumber leftOperand, TValueNumber rightOperand |
        binaryValueNumber(irFunc, opcode, type, leftOperand, rightOperand, instr) and
        result = TBinaryValueNumber(irFunc, opcode, type, leftOperand, rightOperand)
      )
      or
      exists(Opcode opcode, IRType type, TValueNumber operand |
        unaryValueNumber(irFunc, opcode, type, operand, instr) and
        result = TUnaryValueNumber(irFunc, opcode, type, operand)
      )
      or
      exists(
        Opcode opcode, Language::Class baseClass, Language::Class derivedClass, TValueNumber operand
      |
        inheritanceConversionValueNumber(irFunc, opcode, baseClass, derivedClass, operand, instr) and
        result = TInheritanceConversionValueNumber(irFunc, opcode, baseClass, derivedClass, operand)
      )
      or
      exists(
        Opcode opcode, IRType type, int elementSize, TValueNumber leftOperand,
        TValueNumber rightOperand
      |
        pointerArithmeticValueNumber(irFunc, opcode, type, elementSize, leftOperand, rightOperand,
          instr) and
        result =
          TPointerArithmeticValueNumber(irFunc, opcode, type, elementSize, leftOperand, rightOperand)
      )
      or
      exists(IRType type, TValueNumber memOperand, TValueNumber operand |
        loadTotalOverlapValueNumber(irFunc, type, memOperand, operand, instr) and
        result = TLoadTotalOverlapValueNumber(irFunc, type, memOperand, operand)
      )
      or
      // The value number of a copy is just the value number of its source value.
      result = tvalueNumber(instr.(CongruentCopyInstruction).getSourceValue())
    )
  )
}
