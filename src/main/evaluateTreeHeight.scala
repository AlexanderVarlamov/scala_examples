package main

object evaluateTreeHeight extends App {
  scala.io.StdIn.readLine()
  val arrayString = scala.io.StdIn.readLine()
  System.setProperty("Xss", "200000")
  def defineHeight(tree: Node[Int]): Int = {
    if (tree.isLeaf) 1
    else {
      1 + tree.child.map(defineHeight).max
    }
  }

  def makeTree(arrString: String) = {
    val treeArray = arrString.split(" ").filter(_.nonEmpty).map(_.toInt)
    val parents = (for (i <- treeArray.indices) yield (i, treeArray(i))).toMap
    val children = parents.groupBy(_._2).mapValues(x => x.keys.toVector)
    val headIs = Node(children.getOrElse(-1, Vector(-1)).head)

    ifThereChild(headIs, children)
  }

  def ifThereChild(nodeToExplore: Node[Int], children: Map[Int, Vector[Int]]): Node[Int] = {
    children.get(nodeToExplore.content) match {
      case Some(value) => Node(nodeToExplore.content, value.map(x => ifThereChild(Node[Int](x), children)))
      case None => nodeToExplore
    }
  }

  println(defineHeight(makeTree(arrayString)))
}

case class Node[T](content: T, child: Vector[Node[T]] = Vector.empty[Node[T]]) {
  override val toString: String = s"Node($content, ${child.mkString("Array(", ", ", ")")})"
  val isLeaf: Boolean = if (child.isEmpty) true else false
}



