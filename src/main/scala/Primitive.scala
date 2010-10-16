

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

  def cdr(exprs:Expr) = exprs match {
    case ConsExpr(x,xs) => xs
    case _ => throw new Exception("cdr error")
  }

  def minus(exprs:Expr):NumberExpr = exprs match{
    case ListExpr(x:NumberExpr,y:NumberExpr) => NumberExpr(x.num - y.num)
    case _ => throw new Exception("minus error")
  }

  def equal(exprs:Expr):Expr = exprs match {
    case ListExpr(x:NumberExpr, y:NumberExpr) => 
      if (x.num == y.num){
	SymbolExpr("true")
      }else{
	SymbolExpr("false")
      }
    case _ => throw new Exception("equal error")
  }

  def tableRow(tpl:(String, Expr=>Expr)) = {
    val (str, func) = tpl
    (SymbolExpr(str), PrimitiveExpr(func))
  }

  val primitiveMap = List(
    "+" -> plus _,
    "-" -> minus _,
    "*" -> times _,
    "=" -> equal _,
    "null?" -> isNull _,
    "car" -> car _,
    "cdr" -> cdr _
    ).map(tableRow(_)).toMap
}
