/**
 * @name Print Raw IR
 * @description Outputs a representation of the Raw IR graph
 * @id cpp/print-raw-ir
 * @kind graph
 */

import semmle.code.cpp.ir.implementation.raw.PrintIR

class MyConfig extends PrintIRConfiguration {
  override predicate shouldPrintFunction(Function func) {
    func.getFullSignature() = 
//    "JsUtil::Pair<unsigned int, bool, DefaultComparer>::Pair(const Pair<unsigned int, bool, DefaultComparer> &) -> void"
      "IsDenserThan() -> bool"
  }
}
