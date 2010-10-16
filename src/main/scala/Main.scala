
package org.nagoyahackathon.scalalisp

object Main{
  def main(args:Array[String]){
    while(true){
      val line = Console.readLine(">")
      println(line)
      if(line == ""){ return }
      val eval = new LispEval
      val parser = new SExprParser
      println(eval.eval(parser.parse(line)))
    }
  }
}
