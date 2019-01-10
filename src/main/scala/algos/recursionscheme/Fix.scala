package algos.recursionscheme
import cats.Functor

final case class Fix[F[_]](unfix: F[Fix[F]])

sealed trait Stack[A]
final case class Done[A](result: Int)     extends Stack[A]
final case class More[A](a: A, next: Int) extends Stack[A]

object Stack {
  implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
    override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
      sa match {
        case Done(result)  => Done(result)
        case More(a, next) => More(f(a), next)
      }
  }

  def done[A](result: Int = 1): Stack[A] = Done(result)
  def more[A](a: A, next: Int): Stack[A] = More(a, next)
}
