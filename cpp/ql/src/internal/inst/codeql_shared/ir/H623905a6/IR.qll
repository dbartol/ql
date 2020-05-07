///! codeql-shared:ir.IR
///# module<Language, Construction>
///{
private import semmle.code.cpp.ir.internal.IRCppLanguage as Language
private import semmle.code.cpp.ir.implementation.raw.internal.IRConstruction as Construction
///}
///# import IRFunction<Language, Construction>
///{
import internal.inst.codeql_shared.ir.H623905a6.IRFunction
///}
///# import Instruction<Language, Construction>
///{
import internal.inst.codeql_shared.ir.H623905a6.Instruction
///}
///# import IRBlock<Language, Construction>
///{
import internal.inst.codeql_shared.ir.H623905a6.IRBlock
///}
///# import IRVariable<Language, Construction>
///{
import internal.inst.codeql_shared.ir.H623905a6.IRVariable
///}
///# import Operand<Language, Construction>
///{
import internal.inst.codeql_shared.ir.H623905a6.Operand
///}
///# import EdgeKind<Language>
///{
import internal.inst.codeql_shared.ir.H60b116e3.EdgeKind
///}
///# import MemoryAccessKind
///{
import ir.MemoryAccessKind
///}

private newtype TIRPropertyProvider = MkIRPropertyProvider()

/**
 * Class that provides additional properties to be dumped for IR instructions and blocks when using
 * the PrintIR module. Libraries that compute additional facts about IR elements can extend the
 * single instance of this class to specify the additional properties computed by the library.
 */
class IRPropertyProvider extends TIRPropertyProvider {
  string toString() {
    result = "IRPropertyProvider"
  }

  /**
   * Gets the value of the property named `key` for the specified instruction.
   */
  string getInstructionProperty(Instruction instruction, string key) {
    none()
  }

  /**
   * Gets the value of the property named `key` for the specified block.
   */
  string getBlockProperty(IRBlock block, string key) {
    none()
  }
}
