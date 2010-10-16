
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite


class EvalTestSuite extends FunSuite{
  import SExprParser._

  test("Number test"){
    val eval = new LispEval
    assert(eval.eval(NumberExpr(3)) === NumberExpr(3))
  }

  test("String test"){
    val eval = new LispEval
    assert(eval.eval(StringExpr("ABC")) === StringExpr("ABC"))
  }

  test("quote test"){
    val eval = new LispEval
    assert(eval.eval(ListExpr(SymbolExpr("quote"), NumberExpr(3))) === NumberExpr(3))
  }

  test("definition test"){
    val eval = new LispEval
    assert(
      eval.eval(ListExpr(SymbolExpr("define"), SymbolExpr("x"), NumberExpr(3))) === NumberExpr(3))

    assert(eval.eval(SymbolExpr("x")) === NumberExpr(3))
  }


  test("lambda test"){
    val eval = new LispEval
    val LambdaExpr(args, body, _) = eval.eval(ListExpr(SymbolExpr("lambda"), ListExpr(SymbolExpr("x"), SymbolExpr("y")), SymbolExpr("x")))
    assert(args === ListExpr(SymbolExpr("x"), SymbolExpr("y")))
    assert(body === ListExpr(SymbolExpr("x")))

  }


  import Primitive._
  val parser = new SExprParser
  def parse(text:String) = parser.parse(text)

  test("(+ 3 5) === 8"){
    val eval = new LispEval
    assert(eval.eval(ListExpr(SymbolExpr("+"), NumberExpr(3), NumberExpr(5))) === NumberExpr(8))
  }

  test("eval lambda"){
    val eval = new LispEval

    // ((lambda (x) (+ x 1)) 1) === 2
    val lambda = ListExpr(SymbolExpr("lambda"), ListExpr(SymbolExpr("x")), ListExpr(SymbolExpr("+"), SymbolExpr("x"), NumberExpr(1)))
    assert(eval.eval(ListExpr(lambda, NumberExpr(1))) === NumberExpr(2))
  }

}
