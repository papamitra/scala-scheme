
package org.nagoyahackathon.scalalisp

sealed class Expr
case class Symbol(sym:String) extends Expr{
  override def toString = "'%s" format sym
}
case class ListExpr(lst:List[Expr]) extends Expr{
  override def toString = lst map {_.toString} mkString("(", ", ", ")")
}
case class StringExpr(str:String) extends Expr{
  override def toString = "\"%s\"" format str
}
case class Number(num:Int) extends Expr{
  override def toString = num.toString
}
