package algos.recursionscheme
import cats.Functor

sealed trait Exp

final case class Const(x: Int) extends Exp

final case class Add(x1: Exp, x2: Exp) extends Exp

final case class Mul(x1: Exp, x2: Exp) extends Exp

sealed trait ExpF[A]

final case class ConstF[A](x: Int) extends ExpF[A]

final case class AddF[A](x1: A, x2: A) extends ExpF[A]

final case class MulF[A](x1: A, x2: A) extends ExpF[A]

object Exp {

  implicit val expFunctor: Functor[ExpF] = new Functor[ExpF] {
    override def map[A, B](fa: ExpF[A])(f: A => B): ExpF[B] = fa match {
      case ConstF(x)    => constF(x)
      case AddF(x1, x2) => addF(f(x1), f(x2))
      case MulF(x1, x2) => mulF(f(x1), f(x2))
    }
  }

  def const(x: Int): Exp         = Const(x)
  def add(x1: Exp, x2: Exp): Exp = Add(x1, x2)
  def mul(x1: Exp, x2: Exp): Exp = Mul(x1, x2)

  def constF[A](x: Int): ExpF[A]     = ConstF(x)
  def addF[A](x1: A, x2: A): ExpF[A] = AddF(x1, x2)
  def mulF[A](x1: A, x2: A): ExpF[A] = MulF(x1, x2)

  implicit def toExp(i: Int): Exp = const(i)

  val expCoalgebra: Exp => ExpF[Exp] = {
    case Const(x)    => constF(x)
    case Add(x1, x2) => addF(x1, x2)
    case Mul(x1, x2) => mulF(x1, x2)
  }

  val foldExpInt: ExpF[Int] => Int = {
    case ConstF(x)    => x
    case AddF(x1, x2) => x1 + x2
    case MulF(x1, x2) => x1 * x2
  }

  val foldExpStr: ExpF[String] => String = {
    case ConstF(x)    => x.toString
    case AddF(x1, x2) => s"( $x1 + $x2 )"
    case MulF(x1, x2) => s"$x1 * $x2"
  }

  val expAna = ana(expCoalgebra)

  val expCata = cata(foldExpInt)

  val expHyloInt = hylo(foldExpInt)(expCoalgebra)
  val expHyloStr = hylo(foldExpStr)(expCoalgebra)

}
