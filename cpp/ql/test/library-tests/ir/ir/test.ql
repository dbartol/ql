import cpp
import semmle.code.cpp.ir.implementation.unaliased_ssa.IR
import semmle.code.cpp.ir.implementation.aliased_ssa.internal.AliasAnalysis
import semmle.code.cpp.Print

select
  count(IRAutomaticVariable var |
    variableAddressEscapes(var)
  ),
  count(IRAutomaticVariable var)

/*
from LoadInstruction load
where
  exists(VariableAddressInstruction varAddr |
    varAddr = load.getSourceAddress() and
    varAddr.getVariable() instanceof IRAutomaticVariable and
    load.getSourceValueOperand().getDefinitionInstruction() instanceof ChiInstruction
  )
select
  load,
  getIdentityString(load.getEnclosingFunction())
*/