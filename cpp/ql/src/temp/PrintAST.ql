/**
 * @name Print AST
 * @description Outputs a representation of the Abstract Syntax Tree. 
 * @id cpp/print-ast
 * @kind graph
 */

import cpp
import semmle.code.cpp.PrintAST

class MyConfig extends PrintASTConfiguration {
  override predicate shouldPrintFunction(Function func) {
    func.getFullSignature() = 
//    "JsUtil::Pair<unsigned int, bool, DefaultComparer>::Pair(const Pair<unsigned int, bool, DefaultComparer> &) -> void"
//      "IsDenserThan() -> bool"
      "Js::DeferredTypeHandler<DeferredConstructorInitializer, DefaultDeferredTypeFilter, false, (unsigned short)0U, (unsigned short)0U>::EnsureObjectReady(DynamicObject *, DeferredInitializeMode) -> bool"
  }
}
