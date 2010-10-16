
package org.nagoyahackathon.scalalisp

sealed class Expr

case class SymbolExpr(sym: String) extends Expr {
  override def toString = "'%s" format sym
}

case class ConsExpr(car: Expr, cdr: Expr) extends Expr {
  override def toString = "(" + car.toString + " . " + cdr.toString + ")"
}

case object NilExpr extends Expr {
  override def toString = "Nil"
}

case class StringExpr(str: String) extends Expr {
  override def toString = "\"%s\"" format str
}
case class NumberExpr(num: Int) extends Expr {
  override def toString = num.toString
}

case class LambdaExpr(args: Expr, body: Expr, env: Env) extends Expr {
  override def toString = "<Procedure>"
}

case class PrimitiveExpr(proc: Expr => Expr) extends Expr {
  override def toString = "<Primitive>"
}

object ListExpr {
  def unapplySeq(cons: Expr): Option[Seq[Expr]] = cons match {
    case ConsExpr(x, xs) => unapplySeq(xs).map(l => Seq(x) ++ l)
    case NilExpr => Some(List[Expr]())
    case _ => None
  }

  def apply(s: Expr*): Expr = (s :\ (NilExpr: Expr))((x, xs) => ConsExpr(x, xs))
}
