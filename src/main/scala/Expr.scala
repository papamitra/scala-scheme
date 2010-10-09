
package org.nagoyahackathon.scalalisp

sealed class Expr
case class SymbolExpr(sym:String) extends Expr{
  override def toString = "'%s" format sym
}
case class ListExpr(lst:List[Expr]) extends Expr{
  override def toString = lst map {_.toString} mkString("(", ", ", ")")
}
case class StringExpr(str:String) extends Expr{
  override def toString = "\"%s\"" format str
}
case class NumberExpr(num:Int) extends Expr{
  override def toString = num.toString
}

object ListExpr{
  def apply(exprs:Expr*) = new ListExpr(exprs.toList)
}
