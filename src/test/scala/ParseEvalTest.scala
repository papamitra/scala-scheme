package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite

class ParseEvalTestSuite extends FunSuite{

  def eval(str:String)(implicit eval:LispEval):Expr = {
    val parser = new SExprParser
    eval.eval(parser.parse(str))
  }

  test("define and lambda"){
    implicit val e = new LispEval
    eval("(define a 1)")
    assert(eval("((lambda (x) (+ x 1)) a)") === NumberExpr(2))
  }

  test("define func"){
    implicit val e = new LispEval
    eval("(define (func x) (+ x 1))")
    assert(eval("(func 1)") === NumberExpr(2))
  }
  
  test("func no args"){
    implicit val e = new LispEval
    eval("(define (func x) 5)")
    assert(eval("(func 1)") === NumberExpr(5))    
  }

  test("define multi line"){
    implicit val e = new LispEval
    eval("(define (func x) (+ x 1) 10)")
    assert(eval("(func 1)") === NumberExpr(10))
  }

  test("eval if"){
    implicit val e = new LispEval
    assert(eval("(if 1 2 3)") === NumberExpr(2))
    assert(eval("(if 'false 2 3)") === NumberExpr(3))
    assert(eval("(if (if 'false 2) 2 3)") === NumberExpr(3))
  }

  test("factorial"){
    implicit val e = new LispEval
    eval("(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))")
    assert(eval("(fact 5)") === NumberExpr(120))
  }

}
