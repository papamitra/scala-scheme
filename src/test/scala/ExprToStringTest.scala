
package org.nagoyahackathon.scalalisp

import org.scalatest.FunSuite


class ToStringTestSuite extends FunSuite{
  test("Number test"){
    assert(NumberExpr(3).toString === "3")
  }

  test("String test"){
    assert(StringExpr("ABC").toString === "\"ABC\"")
  }

  test("Symbol test"){
    assert(SymbolExpr("ABC").toString === "'ABC")
  }
/*
  test("List test"){
    assert(ListExpr(List(SymbolExpr("ABC"), NumberExpr(3))).toString === "('ABC, 3)")
  }

  test("List test2"){
    assert(ListExpr(List(SymbolExpr("ABC"), NumberExpr(3), ListExpr(List(SymbolExpr("ABC"), NumberExpr(3))))).toString === "('ABC, 3, ('ABC, 3))")
  }
*/
}
