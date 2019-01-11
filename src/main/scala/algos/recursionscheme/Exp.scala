package algos.recursionscheme
import cats.Functor

sealed trait Exp[A]

final case class Const[A](x: Int) extends Exp[A]

final case class Add[A](a: A, x1: Exp[A], x2: Exp[A]) extends Exp[A]

final case class Mul[A](a: A, x1: Exp[A], x2: Exp[A]) extends Exp[A]

object Exp {

  implicit val expFunctor: Functor[Exp] = new Functor[Exp] {
    override def map[A, B](fa: Exp[A])(f: A => B): Exp[B] = fa match {
      case Const(x)       => cons(x)
      case Add(a, x1, x2) => add(f(a), map(x1)(f), map(x2)(f))
      case Mul(a, x1, x2) => mul(f(a), map(x1)(f), map(x2)(f))
    }
  }

  def cons[A](x: Int): Exp[A]                      = Const(x)
  def add[A](a: A, x1: Exp[A], x2: Exp[A]): Exp[A] = Add(a, x1, x2)
  def mul[A](a: A, x1: Exp[A], x2: Exp[A]): Exp[A] = Mul(a, x1, x2)

}
