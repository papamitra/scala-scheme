
package org.nagoyahackathon.scalalisp

sealed class Expr
case class SymbolExpr(sym:String) extends Expr{
  override def toString = "'%s" format sym
}

//case class ListExpr(lst:List[Expr]) extends Expr{
//  override def toString = lst map {_.toString} mkString("(", ", ", ")")
//}

case class ConsExpr(car:Expr, cdr:Expr) extends Expr{
  override def toString = "(" + car.toString + " . " + cdr.toString + ")"
}

case object NilExpr extends Expr{
  override def toString = "Nil"
}

case class StringExpr(str:String) extends Expr{
  override def toString = "\"%s\"" format str
}
case class NumberExpr(num:Int) extends Expr{
  override def toString = num.toString
}
