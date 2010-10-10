package org.nagoyahackathon.scalalisp

private[scalalisp] class Container[T](var x:T)
class UpdatableMap[A, B] (var map:scala.collection.mutable.Map[A, Container[B]]){
  def this() = this(scala.collection.mutable.Map[A, Container[B]]())

  def apply(key:A):B = map(key).x
  def += (kv : (A, B)) = {
    if(map.isDefinedAt(kv._1)) 
      map(kv._1).x = kv._2
    else
      map += (kv._1 -> new Container(kv._2))
    kv._2
  }
  def ++ (xs : Seq[(A, B)]) = new UpdatableMap(map ++ (xs map {case (key, value) => (key, new Container(value))}))
}

class LispEval(var env:UpdatableMap[SymbolExpr, Expr]){


  def this(){
    this(new UpdatableMap[SymbolExpr, Expr]())
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

