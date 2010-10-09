
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite


class EvalTestSuite extends FunSuite{
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
    assert(eval.eval(ListExpr(List(SymbolExpr("quote"), NumberExpr(3)))) === NumberExpr(3))
  }

  test("assign test"){
    val eval = new LispEval
    assert(
      eval.eval(ListExpr(List(
	SymbolExpr("set!"), SymbolExpr("x"), NumberExpr(3)))) === 
      NumberExpr(3))

    assert(eval.eval(SymbolExpr("x")) === NumberExpr(3))
  }

  test("lambda test"){
    val eval = new LispEval
    assert(eval.eval(ListExpr(List(SymbolExpr("lambda"), ListExpr(List(SymbolExpr("x"), SymbolExpr("y"))), ListExpr(List(SymbolExpr("x")))))) === 
      ListExpr(List(SymbolExpr("procedure"), ListExpr(List(SymbolExpr("x"), SymbolExpr("y"))), ListExpr(List(SymbolExpr("x"))))))
  }
}
