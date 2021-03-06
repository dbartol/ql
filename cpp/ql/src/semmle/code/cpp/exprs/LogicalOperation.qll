import semmle.code.cpp.exprs.Expr

/**
 * A C/C++ unary logical operation.
 */
abstract class UnaryLogicalOperation extends UnaryOperation { }

/**
 * A C/C++ logical not expression.
 */
class NotExpr extends UnaryLogicalOperation, @notexpr {
  override string getOperator() { result = "!" }

  override string getCanonicalQLClass() { result = "NotExpr" }

  override int getPrecedence() { result = 15 }
}

/**
 * A C/C++ binary logical operation.
 */
abstract class BinaryLogicalOperation extends BinaryOperation {
  /**
   * Holds if the truth of this binary logical expression having value `wholeIsTrue`
   * implies that the truth of the child expression `part` has truth value `partIsTrue`.
   *
   * For example if the binary operation:
   * ```
   *   x && y
   * ```
   * is true, `x` and `y` must also be true, so `impliesValue(x, true, true)` and
   * `impliesValue(y, true, true)` hold.
   */
  abstract predicate impliesValue(Expr part, boolean partIsTrue, boolean wholeIsTrue);
}

/**
 * A C/C++ logical and expression.
 */
class LogicalAndExpr extends BinaryLogicalOperation, @andlogicalexpr {
  override string getOperator() { result = "&&" }

  override string getCanonicalQLClass() { result = "LogicalAndExpr" }

  override int getPrecedence() { result = 5 }

  override predicate impliesValue(Expr part, boolean partIsTrue, boolean wholeIsTrue) {
    wholeIsTrue = true and partIsTrue = true and part = this.getAnOperand()
    or
    wholeIsTrue = true and
    this.getAnOperand().(BinaryLogicalOperation).impliesValue(part, partIsTrue, true)
  }
}

/**
 * A C/C++ logical or expression.
 */
class LogicalOrExpr extends BinaryLogicalOperation, @orlogicalexpr {
  override string getOperator() { result = "||" }

  override string getCanonicalQLClass() { result = "LogicalOrExpr" }

  override int getPrecedence() { result = 4 }

  override predicate impliesValue(Expr part, boolean partIsTrue, boolean wholeIsTrue) {
    wholeIsTrue = false and partIsTrue = false and part = this.getAnOperand()
    or
    wholeIsTrue = false and
    this.getAnOperand().(BinaryLogicalOperation).impliesValue(part, partIsTrue, false)
  }
}

/**
 * A C/C++ conditional ternary expression.
 */
class ConditionalExpr extends Operation, @conditionalexpr {
  /** Gets the condition of this conditional expression. */
  Expr getCondition() { expr_cond_guard(underlyingElement(this), unresolveElement(result)) }

  override string getCanonicalQLClass() { result = "ConditionalExpr" }

  /** Gets the 'then' expression of this conditional expression. */
  Expr getThen() {
    if this.isTwoOperand()
    then result = this.getCondition()
    else expr_cond_true(underlyingElement(this), unresolveElement(result))
  }

  /** Gets the 'else' expression of this conditional expression. */
  Expr getElse() { expr_cond_false(underlyingElement(this), unresolveElement(result)) }

  /**
   * Holds if this expression used the two operand form `guard ? : false`.
   */
  predicate isTwoOperand() { expr_cond_two_operand(underlyingElement(this)) }

  override string getOperator() { result = "?" }

  override string toString() { result = "... ? ... : ..." }

  override int getPrecedence() { result = 3 }

  override predicate mayBeImpure() {
    this.getCondition().mayBeImpure() or
    this.getThen().mayBeImpure() or
    this.getElse().mayBeImpure()
  }

  override predicate mayBeGloballyImpure() {
    this.getCondition().mayBeGloballyImpure() or
    this.getThen().mayBeGloballyImpure() or
    this.getElse().mayBeGloballyImpure()
  }
}
