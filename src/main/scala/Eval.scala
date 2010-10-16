package org.nagoyahackathon.scalalisp

class LispEval{
  var env = Env.initEnv

  def map(e:Expr)(f:Expr=>Expr):Expr = e match{
    case ConsExpr(x,xs) => ConsExpr(f(x), map(xs)(f))
    case NilExpr => NilExpr
    case _ => throw new Exception("bad map:" + e.toString)
  }

  def apply(proc:Expr, args:Expr) = proc match{
    case PrimitiveExpr(f) => f(args)
    case LambdaExpr(params,body,env) =>
      val exenv = Env.extend(params, args, env)
      evalSequence(body, exenv)
    case _ => throw new Exception("bad apply" + proc.toString)
  }

//  def seqToCons(seq:Seq[Expr]):Expr = (seq :\ (NilExpr:Expr))(ConsExpr(_,_))

  def eval(expr:Expr, env:Env=env):Expr = expr match {
    case num:NumberExpr => num
    case str:StringExpr => str
    case sym:SymbolExpr => env.lookup(sym)
    case ListExpr(SymbolExpr("quote"), x) => x
    case ListExpr(SymbolExpr("define"), vr:SymbolExpr, vl) => env.defVar(vr, eval(vl,env))
    case ConsExpr(SymbolExpr("define"), ConsExpr(ConsExpr(vr:SymbolExpr, params), body)) =>
      env.defVar(vr, LambdaExpr(params, body, env))
    case ListExpr(SymbolExpr("set!"), name : SymbolExpr, value : Expr) =>
      env.setVar(name, eval(value,env))
    case ConsExpr(SymbolExpr("if"), ConsExpr(pred, ConsExpr(conseq, alt))) =>
      if(eval(pred,env) != SymbolExpr("false")){
	eval(conseq,env)
      }else{
	alt match{
	  case ConsExpr(x,_) => eval(x,env)
	  case NilExpr => SymbolExpr("false")
	  case _ => throw new Exception("if error")
	}
      }
    case ConsExpr(SymbolExpr("lambda") ,ConsExpr(args:Expr, body)) => LambdaExpr(args, body, env)
    case ConsExpr(operator, operands) =>
      apply(eval(operator,env), map(operands)(eval(_,env)) )

    case _ => throw new Exception("Unknown Token:" + expr)
  }

  def evalSequence(expr:Expr, env:Env):Expr = expr match{
    case ConsExpr(x,NilExpr) => eval(x,env)
    case ConsExpr(x, xs) =>
      eval(x,env)
      evalSequence(xs,env)
    case _ => throw new Exception("evalSequence Error")
  }

}

