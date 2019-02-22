import cpp
import semmle.code.cpp.ir.IR
import semmle.code.cpp.Print

from PhiInstruction instr
select instr, getIdentityString(instr.getEnclosingFunction()), instr.getUniqueId()
