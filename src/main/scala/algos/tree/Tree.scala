package algos.tree

import cats.Show
import cats.implicits._
import cats.data._

sealed trait Tree[+T] {

  def +[S >: T](x: S)(implicit ord: Ordering[S]): Tree[S]

}

case object Empty extends Tree[Nothing] {
  override def +[T](x: T)(implicit ord: Ordering[T]): Tree[T] = Node(Empty, x, Empty)
}

case class Node[T](l: Tree[T], value: T, r: Tree[T]) extends Tree[T] {
  override def +[S >: T](x: S)(implicit ord: Ordering[S]): Tree[S] = ord.compare(value, x) match {
    case 0          => this
    case i if i > 0 => Node(l + x, value, r)
    case _          => Node(l, value, r + x)
  }
}

object Tree {

  private def toString[T: Show](t: Tree[T]): String = t match {
    case Empty         => "_"
    case Node(l, v, r) => s"(${toString(l)},${v.show},${toString(r)})"
  }

  implicit def showTree[T: Show]: Show[Tree[T]] = Show.show(toString(_))

  def empty[T]: Tree[T] = Empty

  def apply[T: Ordering](v: T*): Tree[T] = v.foldLeft(empty[T])(_ + _)

  def foreach[T, U](t: Tree[T])(f: T => U): Unit = t match {
    case Empty => ()
    case Node(l, v, r) =>
      foreach(l)(f)
      f(v)
      foreach(r)(f)
  }

}
