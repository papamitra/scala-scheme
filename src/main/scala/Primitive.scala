

package org.nagoyahackathon.scalalisp

object Primitive{
//  def num(e:Expr) = e match { case NumberExpr(n) => n}

  def plus(exprs:Expr):NumberExpr = exprs match {
    case ConsExpr(x:NumberExpr, xs) =>
      NumberExpr(x.num + plus(xs).num)
    case NilExpr => NumberExpr(0)
    case _ => throw new Exception("plus type miss match")
  }

  def times(exprs:Expr):NumberExpr = exprs match {
    case ConsExpr(x:NumberExpr, xs) =>
      NumberExpr(x.num * times(xs).num)
    case NilExpr => NumberExpr(1)
    case _ => throw new Exception("times type miss match")
  }

  def isNull(exprs:Expr) = exprs match {
    case NilExpr => SymbolExpr("#t")
    case _ => SymbolExpr("#f")
  }

  def car(exprs:Expr) = exprs match {
    case ConsExpr(x,xs) => x
    case _ => throw new Exception("car error")
  }

  implicit def tableItem(tpl:(String, Expr=>Expr)) = {
    val (str, func) = tpl
    (SymbolExpr(str), PrimitiveExpr(func))
  }

  val primitiveMap = Map[SymbolExpr, Expr](
    "+" -> plus _,
    "*" -> times _,
    "null?" -> isNull _
    )
}
