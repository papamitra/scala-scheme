
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
    assert(eval.eval(listExpr(List(SymbolExpr("quote"), NumberExpr(3)))) === NumberExpr(3))
  }

  test("assign test"){
    val eval = new LispEval
    assert(
      eval.eval(listExpr(List(
	SymbolExpr("set!"), SymbolExpr("x"), NumberExpr(3)))) === NumberExpr(3))

    assert(eval.eval(SymbolExpr("x")) === NumberExpr(3))
  }

  test("lambda test"){
    val eval = new LispEval
    assert(eval.eval(listExpr(List(SymbolExpr("lambda"), listExpr(List(SymbolExpr("x"), SymbolExpr("y"))), SymbolExpr("x")))) === 
      listExpr(List(SymbolExpr("procedure"), listExpr(List(SymbolExpr("x"), SymbolExpr("y"))), SymbolExpr("x"))))
  }
/*
  test("apply test"){
    import scala.collection.mutable.Map
    val eval = new LispEval(Map(SymbolExpr("func") ->  ListExpr(SymbolExpr("procedure"), ListExpr(SymbolExpr("x"), SymbolExpr("y")), SymbolExpr("x"))))
    assert(eval.eval(ListExpr(SymbolExpr("func"), SymbolExpr("test"), SymbolExpr("test2"))) === SymbolExpr("test"))
  }
*/
  test("apply lambda test"){
    val eval = new LispEval
    assert(eval.eval(listExpr(List(SymbolExpr("set!"), SymbolExpr("func"), listExpr(List(SymbolExpr("lambda"), listExpr(List(SymbolExpr("x"), SymbolExpr("y"))), SymbolExpr("x")))))) === 
      listExpr(List(SymbolExpr("procedure"), listExpr(List(SymbolExpr("x"), SymbolExpr("y"))), SymbolExpr("x"))))
    assert(eval.eval(listExpr(List(SymbolExpr("func"), SymbolExpr("test"), SymbolExpr("test2")))) === SymbolExpr("test"))
  }

  test("+ test"){
    val eval = new LispEval
//    assert(eval.eval(ListExpr(SymbolExpr("+"), NumberExpr(3), NumberExpr(5))) === NumberExpr(8))
    import Primitive._
    val parser = new SExprParser
    def parse(text:String) = parser.parse(text)

    println("eval:"+eval.eval(parse("(+ 1 2 7)")))
  }

  
}


