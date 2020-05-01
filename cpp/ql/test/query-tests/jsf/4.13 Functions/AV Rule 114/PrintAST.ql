/**
 * @kind graph
 */

import semmle.code.cpp.PrintAST

class Configuration extends PrintASTConfiguration {
  override predicate shouldPrintFunction(Function f) {
    f.getName() = "std_json_parse"
  }
}
