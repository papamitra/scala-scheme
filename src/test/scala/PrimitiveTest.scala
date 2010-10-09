
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite

class PrimitiveTestSuite extends FunSuite{  
  import Primitive._
  val parser = new SExprParser

  def parse(text:String) = parser.parse(text)

  test("plus"){
    assert(plus(parse("(1 2 3)")) === NumberExpr(6))
  }

  test("times"){
    assert(times(parse("(1 2 2)")) === NumberExpr(4))
    assert(times(parse("(1 2 0)")) === NumberExpr(0))
  }

  test("isNull"){
    assert(isNull(parse("()")) === parse("#t"))
    assert(isNull(parse("(1 2 3)")) === parse("#f"))
    assert(isNull(parse("'a")) === parse("#f"))
    assert(isNull(parse("0")) === parse("#f"))
  }

  test("car"){
    assert(car(parse("(1 2 3)")) === parse("1"))
  }
}
