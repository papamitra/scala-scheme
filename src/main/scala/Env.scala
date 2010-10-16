
package org.nagoyahackathon.scalalisp

import scala.collection.mutable.Map

sealed trait Env{
  def lookup(v:SymbolExpr):Expr
  def defVar(v:SymbolExpr, e:Expr):Expr
  def setVar(v:SymbolExpr, e:Expr):Expr
  def scan(v:SymbolExpr):Option[Expr]
}

object Env{

  private class Frame(varTable:Map[SymbolExpr, Expr], Outer:Env) extends Env {
    def scan(v:SymbolExpr) = varTable.get(v) orElse Outer.scan(v)
    def lookup(v:SymbolExpr) = scan(v) match {
      case Some(e) => e
      case _ => throw new Exception("var not found: " + v.toString)
    }

    def defVar(v:SymbolExpr, e:Expr)={varTable(v) = e;e}
    def setVar(v:SymbolExpr, e:Expr)={ scan(v) match{
      case Some(_) => varTable(v) = e;e
      case _ => throw new Exception("var not found: " + v.toString)
    }}
  }

  private object EmptyEnv extends Env{
    def lookup(v:SymbolExpr) = throw new Exception("var not found: " + v.toString)
    def scan(v:SymbolExpr) = None
    def defVar(v:SymbolExpr, e:Expr)= throw new Exception("Empty Env")
    def setVar(v:SymbolExpr, e:Expr)= throw new Exception("Empty Env")
  }

  def zip(a:Expr, b:Expr):List[(SymbolExpr,Expr)] = (a,b) match{
    case (ConsExpr(x:SymbolExpr,xs), ConsExpr(y,ys)) => List((x,y)) ++ zip(xs,ys)
    case (NilExpr, NilExpr) => Nil
    case _ => throw new Exception("bad zip")
  }

  def extend(vars:Expr, vals:Expr, env:Env):Env = new Frame(Map() ++= zip(vars,vals), env)

  def initEnv:Env = new Frame(Map() ++= Primitive.primitiveMap, EmptyEnv)
}
