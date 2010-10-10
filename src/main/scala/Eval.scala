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
  import SExprParser._

  def this(){
    this(new UpdatableMap[SymbolExpr, Expr]())
  }

  def zip(a:Expr, b:Expr):List[(SymbolExpr,Expr)] = (a,b) match{
    case (ConsExpr(x:SymbolExpr,xs), ConsExpr(y,ys)) => List((x,y)) ++ zip(xs,ys)
    case _ => Nil
  }

  val operatorMap:Map[String, Expr => Expr] = Primitive.primitiveMap

  def applyOperator(func: Expr, args: Expr):Expr = func match{
    case ConsExpr(SymbolExpr("procedure"), ConsExpr(argsName:Expr,ConsExpr(body,NilExpr))) => 
      val evaluator = new LispEval(env ++ zip(argsName,args))
      evaluator.eval(body)
    case sym@SymbolExpr(str) =>
      operatorMap.get(str) match {
	case Some(primFunc) => primFunc(args)
	case None => applyOperator(env(sym), args)
      }
  }

  def eval(expr : Expr): Expr = expr match {
    case num:NumberExpr => num
    case str:StringExpr => str
    case sym:SymbolExpr => env(sym)
    case ConsExpr(SymbolExpr("quote"), ConsExpr(x,_)) => x
    case ConsExpr(SymbolExpr("set!"), ConsExpr(name : SymbolExpr, ConsExpr(value : Expr, NilExpr))) => 
      env += (name -> eval(value))
      env(name)
    case ConsExpr(SymbolExpr("lambda") ,ConsExpr(args : Expr, ConsExpr(body : Expr, NilExpr))) => listExpr(List(SymbolExpr("procedure"), args, body))
    case ConsExpr(operator : SymbolExpr, rest) =>
      applyOperator(operator, rest)

    case _ => throw new Exception("Unknown Token:" + expr)
  }
}

