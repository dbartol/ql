import cpp
import semmle.code.cpp.ir.implementation.raw.IR
import semmle.code.cpp.ir.IRConfiguration

class MyConfig extends IRConfiguration {
    override predicate shouldCreateIRForFunction(Function f) {
        f.getName() = "f_with_op"
    }
}

from NewArrayExpr e
where e.getLocation().getStartLine() = 20
select e, e.getAllocator().getParameter(0).getType()

