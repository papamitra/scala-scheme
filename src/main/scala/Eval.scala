package org.nagoyahackathon.scalalisp

class LispEval(var env:scala.collection.mutable.Map[SymbolExpr, Expr]){
  def this(){
    this(scala.collection.mutable.Map[SymbolExpr, Expr]())
  }

  def applyOperator(func: Expr, args: List[Expr]) = {
    func match {
      case ListExpr(SymbolExpr("procedure") :: ListExpr(argsName:List[SymbolExpr]) :: body :: Nil) => 
	val eval = new LispEval(env ++ (argsName zip args))
	eval.eval(body)
      case _ => throw new Exception("Not Function: "+func)
    }
  }

  def eval(expr : Expr): Expr = expr match {
    case num:NumberExpr => num
    case str:StringExpr => str
    case sym:SymbolExpr => env(sym)
    case ListExpr(SymbolExpr("quote") :: second :: Nil) => second
    case ListExpr(SymbolExpr("set!") :: (name : SymbolExpr) :: (value : Expr) :: Nil) => 
      env += (name -> eval(value))
      env(name)
    case ListExpr(SymbolExpr("lambda") :: (args : ListExpr) :: (body : Expr) :: Nil) => ListExpr(SymbolExpr("procedure"), args, body)
    case ListExpr((operator : Expr) :: rest) =>
      applyOperator(eval(operator), rest)

    case _ => throw new Exception("Unknown Token:" + expr)
  }
}

