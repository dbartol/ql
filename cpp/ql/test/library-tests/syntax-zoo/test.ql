/**
 * @kind graph
 */
 
import cpp
import semmle.code.cpp.ir.implementation.raw.IR
import semmle.code.cpp.ir.IRConfiguration
import semmle.code.cpp.ir.implementation.raw.PrintIR

class MyConfig extends IRConfiguration {
    override predicate shouldCreateIRForFunction(Function f) {
        f.getName() = "f_with_op"
    }
}
