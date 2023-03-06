package main

object evaluateTreeHeight2 extends App {
  scala.io.StdIn.readLine()
  val arrayString = scala.io.StdIn.readLine()
  val nodes2 = arrayString.split(" ").filter(_.nonEmpty).map(x=>(x.toInt, -1))

  def height(i: Int): Int = {
    if (nodes2(i)._2 == -1) {
      nodes2(i)._1 match {
        case -1 => nodes2(i) = (nodes2(i)._1, 1); 1
        case _ => 1 + height(nodes2(i)._1)
      }
    } else nodes2(i)._2
  }

  val result = for (i <- nodes2.indices) yield height(i)
  println(nodes2.mkString(" "))
  println(result.max)
}




