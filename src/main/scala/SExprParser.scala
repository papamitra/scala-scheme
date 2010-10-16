
package org.nagoyahackathon.scalalisp

import scala.util.parsing.combinator._


class SExprParser extends JavaTokenParsers {
  import SExprParser._
  val strregexp = """^"(.*)"$""".r

  def expr:Parser[Expr] = symbol | string | num | quoted | nil | list
  def string:Parser[StringExpr] = stringLiteral ^^ { case strregexp(str) => StringExpr(str)}
  def num:Parser[NumberExpr] = decimalNumber ^^ { case n => NumberExpr(n.toInt)}
  def nil:Parser[Expr] = "("~")" ^^ {case _ => NilExpr}
  def list:Parser[Expr] = "("~> rep(expr) <~")" ^^ {case e => listExpr(e)}
  def quoted:Parser[Expr] = "'"~>expr ^^ {case e => ConsExpr(SymbolExpr("quote"), ConsExpr(e, NilExpr))}
  def symbol:Parser[SymbolExpr] = """[a-zA-Z_+-/*%$#=]\w*""".r ^^ { case e => SymbolExpr(e.toString)}

  def parse(text:String) = parseAll(expr, text) match{
    case Success(e, _) => e
    case _ => throw new Exception("parse error")
  }

}

object SExprParser{
  def listExpr(l:List[Expr]):Expr = l match {
    case x :: xs => ConsExpr(x, listExpr(xs))
    case _ => NilExpr
  }
}
