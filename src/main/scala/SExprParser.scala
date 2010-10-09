
package org.nagoyahackathon.scalalisp

import scala.util.parsing.combinator._


class SExprParser extends JavaTokenParsers {

  val strregexp = """^"(.*)"$""".r

  def expr:Parser[Expr] = symbol | string | num | list | quoted
  def string:Parser[StringExpr] = stringLiteral ^^ { case strregexp(str) => StringExpr(str)}
  def num:Parser[NumberExpr] = decimalNumber ^^ { case n => NumberExpr(n.toInt)}
  def list:Parser[ListExpr] = "("~> rep(expr) <~")" ^^ { case e => ListExpr(List() ++ e)}
  def quoted:Parser[ListExpr] = "'"~>expr ^^ {case e => ListExpr(List(SymbolExpr("quote"), e))}
  def symbol:Parser[SymbolExpr] = """[a-zA-Z_+-/*%$]\w*""".r ^^ { case e => SymbolExpr(e.toString)}

  def parse(text:String) = parseAll(expr, text) match{
    case Success(e, _) => e
    case _ => throw new Exception("parse error")
  }

}
