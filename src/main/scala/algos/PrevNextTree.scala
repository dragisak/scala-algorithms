package algos

case class Node(
    value: Int,
    left: Option[Node] = None,
    right: Option[Node] = None,
    prev: Option[Node] = None, // TODO
    next: Option[Node] = None
) {
  def +(x: Int): Node =
    if (x < value) {
      val a = left match {
        case Some(n) => copy(left = Some(n + x))
        case None    => copy(left = Some(Node(x)))
      }
    } else if (x > value) {
      right match {
        case Some(n) => copy(right = Some(n + x))
        case None    => copy(right = Some(Node(x)))
      }
    } else {
      this
    }

  def ++(xx: Iterable[Int]): Node = xx.foldLeft(this)(_ + _)

  override def toString: String =
    s"$value (${left.map(_.toString).getOrElse(" ")},${right.map(_.toString).getOrElse(" ")})"
}

object Tree {

  def of(i: Int, ii: Int*): Node = Node(i) ++ ii

}
