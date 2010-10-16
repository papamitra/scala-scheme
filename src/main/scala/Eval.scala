package org.nagoyahackathon.scalalisp

class LispEval{
  import SExprParser._

  var env = Env.initEnv

  def map(e:Expr)(f:Expr=>Expr):Expr = e match{
    case ConsExpr(x,xs) => ConsExpr(f(x), map(xs)(f))
    case NilExpr => NilExpr
    case _ => throw new Exception("bad map:" + e.toString)
  }

  def applyProc(proc:Expr, args:Expr) = proc match{
    case PrimitiveExpr(f) => f(args)
    case ProcExpr(params,body,env) =>
      val exenv = Env.extend(params, args, env)
      eval(body, exenv)
    case _ => throw new Exception("bad apply" + proc.toString)
  }

  def eval(expr:Expr, env:Env=env):Expr = expr match {
    case num:NumberExpr => num
    case str:StringExpr => str
    case sym:SymbolExpr => env.lookup(sym)
    case ListExpr(SymbolExpr("quote"), x) => x
    case ListExpr(SymbolExpr("define"), vr:SymbolExpr, vl) => env.defVar(vr, eval(vl,env))
    case ListExpr(SymbolExpr("set!"), name : SymbolExpr, value : Expr) => 
      env.setVar(name, eval(value,env))
    case ListExpr(SymbolExpr("lambda") ,args:Expr, body:Expr) => ProcExpr(args, body, env)
    case ConsExpr(operator, operands) =>
      applyProc(eval(operator,env), map(operands)(eval(_,env)) )

    case _ => throw new Exception("Unknown Token:" + expr)
  }
}

