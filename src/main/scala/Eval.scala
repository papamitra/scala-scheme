package org.nagoyahackathon.scalalisp

class LispEval{
  var env =  scala.collection.mutable.Map[SymbolExpr, Expr]()

  def eval(expr : Expr): Expr = expr match {
    case num:NumberExpr => num
    case str:StringExpr => str
    case sym:SymbolExpr => env(sym)
    case ListExpr(SymbolExpr("quote") :: second :: Nil) => second
    case ListExpr(SymbolExpr("set!") :: (name : SymbolExpr) :: (value : Expr) :: Nil) => 
      env += (name -> eval(value))
      env(name)
    case ListExpr(SymbolExpr("lambda") :: (args : ListExpr) :: (body : ListExpr) :: Nil) => ListExpr(List(SymbolExpr("procedure"), args, body))
    case _ => throw new Exception("Unknown Token")
  }
}

