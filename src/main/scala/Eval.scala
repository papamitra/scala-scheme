package org.nagoyahackathon.scalalisp

class LispEval(var env:scala.collection.mutable.Map[SymbolExpr, Expr]){
  def this(){
    this(scala.collection.mutable.Map[SymbolExpr, Expr]())
  }

  val operatorMap:Map[String, Expr => Expr] = Primitive.primitiveMap

  def applyOperator(func: Expr, args: List[Expr]):Expr = func match{
    case ListExpr(SymbolExpr("procedure") :: ListExpr(argsName:List[SymbolExpr]) :: body :: Nil) => 
      val evaluator = new LispEval(env ++ (argsName zip args))
      evaluator.eval(body)
    case sym@SymbolExpr(str) =>
      operatorMap.get(str) match {
	case Some(primFunc) => primFunc(ListExpr(args))
	case None => applyOperator(env(sym), args)
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
    case ListExpr((operator : SymbolExpr) :: rest) =>
      applyOperator(operator, rest)

    case _ => throw new Exception("Unknown Token:" + expr)
  }
}

