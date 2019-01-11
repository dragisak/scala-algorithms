package algos.dag

case class Node(in: Set[Int], out: Set[Int])

case class Dag(nodes: Array[Node]) {
  def add(from: Int, to: Int): Dag = {
    val starNode = nodes(from)
    val n1       = nodes.updated(from, starNode.copy(out = starNode.out + to))

    val endNode = nodes(to)
    Dag(n1.updated(to, endNode.copy(in = endNode.in + from)))
  }

  private def pathsFrom(previous: List[Int]): List[List[Int]] = {
    val node = nodes(previous.head) // always non empty
    if (node.out.isEmpty) {
      List(previous)
    } else {
      node.out.toList.flatMap(next => pathsFrom(next :: previous))
    }
  }

  def paths: List[List[Int]] = {
    // Can do because acyclic
    val startNodes = nodes.toList.zipWithIndex.filter(_._1.in.isEmpty).map(_._2)
    startNodes.flatMap(idx => pathsFrom(idx :: Nil)).map(_.reverse)
  }

}

object Dag {

  private val empty = Node(Set.empty, Set.empty)

  private def init(n: Int): Dag = Dag(Array.fill(n)(empty))

  def apply(numberOfNodes: Int, edges: List[(Int, Int)]): Dag =
    edges.foldLeft(init(numberOfNodes)) {
      case (d, (from, to)) => d.add(from, to)
    }

}

object Solution {

  def main(args: Array[String]): Unit = {
    val d = Dag(
      numberOfNodes = 7,
      edges = List(
        (0, 1),
        (0, 2),
        (1, 3),
        (1, 5),
        (2, 5),
        (6, 2)
      )
    )

    println(d.paths.map(_.mkString("->")).mkString("\n"))
  }
}
