
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite
import scala.util.parsing.combinator._

class ParserTestSuite extends FunSuite{  
  val parser = new SExprParser

  def listGen(exprs:Expr*) = {
    def proc(lst:List[Expr]):Expr = lst match{
      case x :: xs => ConsExpr(x, proc(xs))
      case Nil => NilExpr
    }
    proc(exprs.toList)
  }

  test("parse string"){
    assert(StringExpr("test") === parser.parse("\"test\""))
    assert(StringExpr("") === parser.parse("\"\""))
  }

  test("parse number"){
    assert(NumberExpr(123) === parser.parse("123"))
    assert(NumberExpr(0) === parser.parse("0"))
  }

  test("parse Nil"){
    assert(NilExpr === parser.parse("()"))
  }

  test("parse list"){
    assert(ConsExpr(StringExpr("test"), ConsExpr(NumberExpr(1), NilExpr)) === parser.parse("(\"test\" 1)"))
    assert(listGen(listGen(SymbolExpr("lambda"), listGen(SymbolExpr("x")), listGen(SymbolExpr("+"), SymbolExpr("x"), NumberExpr(1))), NumberExpr(1)) === parser.parse("((lambda (x) (+ x 1)) 1)"))
  }

  test("parse quote"){
    assert(ConsExpr(SymbolExpr("quote"), ConsExpr(ConsExpr(NumberExpr(1), ConsExpr(NumberExpr(2), NilExpr)),NilExpr)) === 
      parser.parse("'(1 2)"))
  }

  test("parse all"){
    assert(ConsExpr(SymbolExpr("apply"), ConsExpr(SymbolExpr("+"),
		    ConsExpr(
		      ConsExpr(SymbolExpr("quote"),
			     ConsExpr(ConsExpr(NumberExpr(1), ConsExpr(NumberExpr(2), ConsExpr(NumberExpr(3),NilExpr))), NilExpr)),NilExpr))) ===
			       parser.parse("(apply + '(1 2 3))"))

/*    assert(ListExpr(SymbolExpr("apply"), SymbolExpr("+"),
		    ListExpr(SymbolExpr("quote"),
			     ListExpr(NumberExpr(1), NumberExpr(2), NumberExpr(3)))) ===
			   parser.parse("""(apply + 
					'(1
					2
					3))"""))
					*/
  }

}
