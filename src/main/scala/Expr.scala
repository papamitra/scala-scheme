
package org.nagoyahackathon.scalalisp

sealed class Expr
case class Symbol(sym:String) extends Expr
case class ListExpr(lst:List[Expr]) extends Expr
case class StringExpr(str:String) extends Expr
case class Number(num:Int) extends Expr
