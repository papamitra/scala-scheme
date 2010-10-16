
package org.nagoyahackathon.scalalisp

object Main{
  def main(args:Array[String]){
    val eval = new LispEval
    val parser = new SExprParser

    while(true){
      val line = Console.readLine("REPL>")
      println(line)
      if(line == ""){ return }
      println(eval.eval(parser.parse(line)))
    }
  }
}
