
import sbt._

class Project(info:ProjectInfo) extends DefaultProject(info){
  val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.final"
}

