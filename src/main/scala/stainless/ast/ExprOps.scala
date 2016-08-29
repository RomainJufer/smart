/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package ast

trait ExprOps extends inox.ast.ExprOps {
  protected val trees: Trees
  import trees._

  protected class VariableExtractor extends super.VariableExtractor {
    override def unapply(e: Expr): Option[(Set[Variable], Set[Variable])] = e match {
      case MatchExpr(_, cases) => Some((Set.empty, cases.flatMap(_.pattern.binders).map(_.toVariable).toSet))
      case _ => super.unapply(e)
    }
  }

  override val VariableExtractor = new VariableExtractor

  /* =================
   * Body manipulation
   * ================= */

  /** Returns whether a particular [[Expressions.Expr]] contains specification
    * constructs, namely [[Expressions.Require]] and [[Expressions.Ensuring]].
    */
  def hasSpec(e: Expr): Boolean = e match {
    case Require(_, _) => true
    case Ensuring(_, _) => true
    case Let(i, e, b) => hasSpec(b)
    case _ => false
  }

  /** Replaces the precondition of an existing [[Expressions.Expr]] with a new one.
    *
    * If no precondition is provided, removes any existing precondition.
    * Else, wraps the expression with a [[Expressions.Require]] clause referring to the new precondition.
    *
    * @param expr The current expression
    * @param pred An optional precondition. Setting it to None removes any precondition.
    * @see [[Expressions.Ensuring]]
    * @see [[Expressions.Require]]
    */
  def withPrecondition(expr: Expr, pred: Option[Expr]): Expr = (pred, expr) match {
    case (Some(newPre), Require(pre, b))              => Require(newPre, b)
    case (Some(newPre), Ensuring(Require(pre, b), p)) => Ensuring(Require(newPre, b), p)
    case (Some(newPre), Ensuring(b, p))               => Ensuring(Require(newPre, b), p)
    case (Some(newPre), Let(i, e, b)) if hasSpec(b)   => Let(i, e, withPrecondition(b, pred))
    case (Some(newPre), b)                            => Require(newPre, b)
    case (None, Require(pre, b))                      => b
    case (None, Ensuring(Require(pre, b), p))         => Ensuring(b, p)
    case (None, Let(i, e, b)) if hasSpec(b)           => Let(i, e, withPrecondition(b, pred))
    case (None, b)                                    => b
  }

  /** Replaces the postcondition of an existing [[Expressions.Expr]] with a new one.
    *
    * If no postcondition is provided, removes any existing postcondition.
    * Else, wraps the expression with a [[Expressions.Ensuring]] clause referring to the new postcondition.
    *
    * @param expr The current expression
    * @param oie An optional postcondition. Setting it to None removes any postcondition.
    * @see [[Expressions.Ensuring]]
    * @see [[Expressions.Require]]
    */
  def withPostcondition(expr: Expr, oie: Option[Lambda]): Expr = (oie, expr) match {
    case (Some(npost), Ensuring(b, post))          => Ensuring(b, npost)
    case (Some(npost), Let(i, e, b)) if hasSpec(b) => Let(i, e, withPostcondition(b, oie))
    case (Some(npost), b)                          => Ensuring(b, npost)
    case (None, Ensuring(b, p))                    => b
    case (None, Let(i, e, b)) if hasSpec(b)        => Let(i, e, withPostcondition(b, oie))
    case (None, b)                                 => b
  }

  /** Adds a body to a specification
    *
    * @param expr The specification expression [[Expressions.Ensuring]] or [[Expressions.Require]]. If none of these, the argument is discarded.
    * @param body An option of [[Expressions.Expr]] possibly containing an expression body.
    * @return The post/pre condition with the body. If no body is provided, returns [[Expressions.NoTree]]
    * @see [[Expressions.Ensuring]]
    * @see [[Expressions.Require]]
    */
  def withBody(expr: Expr, body: Expr): Expr = expr match {
    case Let(i, e, b) if hasSpec(b)      => Let(i, e, withBody(b, body))
    case Require(pre, _)                 => Require(pre, body)
    case Ensuring(Require(pre, _), post) => Ensuring(Require(pre, body), post)
    case Ensuring(_, post)               => Ensuring(body, post)
    case _                               => body
  }

  /** Extracts the body without its specification
    *
    * [[Expressions.Expr]] trees contain its specifications as part of certain nodes.
    * This function helps extracting only the body part of an expression
    *
    * @return An option type with the resulting expression if not [[Expressions.NoTree]]
    * @see [[Expressions.Ensuring]]
    * @see [[Expressions.Require]]
    */
  def withoutSpec(expr: Expr): Option[Expr] = expr match {
    case Let(i, e, b)                    => withoutSpec(b).map(Let(i, e, _))
    case Require(pre, b)                 => Option(b).filterNot(_.isInstanceOf[NoTree])
    case Ensuring(Require(pre, b), post) => Option(b).filterNot(_.isInstanceOf[NoTree])
    case Ensuring(b, post)               => Option(b).filterNot(_.isInstanceOf[NoTree])
    case b                               => Option(b).filterNot(_.isInstanceOf[NoTree])
  }

  /** Returns the precondition of an expression wrapped in Option */
  def preconditionOf(expr: Expr): Option[Expr] = expr match {
    case Let(i, e, b)                 => preconditionOf(b).map(Let(i, e, _).copiedFrom(expr))
    case Require(pre, _)              => Some(pre)
    case Ensuring(Require(pre, _), _) => Some(pre)
    case b                            => None
  }

  /** Returns the postcondition of an expression wrapped in Option */
  def postconditionOf(expr: Expr): Option[Expr] = expr match {
    case Let(i, e, b)      => postconditionOf(b).map(Let(i, e, _).copiedFrom(expr))
    case Ensuring(_, post) => Some(post)
    case _                 => None
  }

  /** Returns a tuple of precondition, the raw body and the postcondition of an expression */
  def breakDownSpecs(e: Expr) = (preconditionOf(e), withoutSpec(e), postconditionOf(e))
}
