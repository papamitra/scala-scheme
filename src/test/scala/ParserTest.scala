
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite
import scala.util.parsing.combinator._

class ParserTestSuite extends FunSuite{  
  val parser = new SExprParser

  test("parse string"){
    assert(StringExpr("test") === parser.parse("\"test\""))
    assert(StringExpr("") === parser.parse("\"\""))
  }

  test("parse number"){
    assert(NumberExpr(123) === parser.parse("123"))
    assert(NumberExpr(0) === parser.parse("0"))
  }

  test("parse list"){
    assert(ListExpr(StringExpr("test"), NumberExpr(1)) === parser.parse("(\"test\" 1)"))
    assert(ListExpr() === parser.parse("()"))
  }

  test("parse quote"){
    assert(ListExpr(SymbolExpr("quote"), ListExpr(NumberExpr(1), NumberExpr(2), NumberExpr(3))) === 
      parser.parse("'(1 2 3)"))
  }

  test("parse all"){
    assert(ListExpr(SymbolExpr("apply"), SymbolExpr("+"),
		    ListExpr(SymbolExpr("quote"),
			     ListExpr(NumberExpr(1), NumberExpr(2), NumberExpr(3)))) ===
			       parser.parse("(apply + '(1 2 3))"))

    assert(ListExpr(SymbolExpr("apply"), SymbolExpr("+"),
		    ListExpr(SymbolExpr("quote"),
			     ListExpr(NumberExpr(1), NumberExpr(2), NumberExpr(3)))) ===
			   parser.parse("""(apply + 
					'(1
					2
					3))"""))
  }
}
