

package org.nagoyahackathon.scalalisp

object Primitive{
//  def num(e:Expr) = e match { case NumberExpr(n) => n}

  def plus(exprs:Expr) = {
    println("plus"+exprs)
    exprs match {
    case ListExpr(xs:List[NumberExpr]) =>
      NumberExpr((0 /: xs)(_ + _.num))
    case _ => throw new Exception("plus type miss match")
  }}

  def times(exprs:Expr) = exprs match {
    case ListExpr(xs:List[NumberExpr]) =>
      NumberExpr((1 /: xs)(_ * _.num))
    case _ => throw new Exception("time type miss match")
  }

  def isNull(exprs:Expr) = exprs match {
    case ListExpr(List()) => SymbolExpr("#t")
    case _ => SymbolExpr("#f")
  }

  def car(exprs:Expr) = exprs match {
    case ListExpr(xs:List[Expr]) =>
      xs.head
    case _ => throw new Exception("car error")
  }

  implicit def tableItem(tpl:(String, Expr=>Expr)) = {
    val (str, func) = tpl
    (SymbolExpr(str), func)
  }

  val primitiveMap = Map(
    "+" -> plus _,
    "*" -> times _,
    "null?" -> isNull _
    )
}
