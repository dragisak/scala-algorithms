package algos
import cats.Functor
import cats.syntax.functor._

package object recursionscheme {

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  def ana[F[_]: Functor, A](f: A => F[A]): A => Fix[F] =
    a => Fix(f(a) map ana(f))

  def cata[F[_]: Functor, A](f: F[A] => A): Fix[F] => A =
    fix => f(fix.unfix map cata(f))

  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    a => f(g(a) map hylo(f)(g))

  def para[F[_]: Functor, A](f: F[(Fix[F], A)] => A): Fix[F] => A =
    fix => f(fix.unfix.map(fix => fix -> para(f).apply(fix)))

  def apo[F[_]: Functor, A](f: A => F[Either[Fix[F], A]]): A => Fix[F] =
    a =>
      Fix(
        f(a) map {
          case Left(fix) => fix
          case Right(aa) => apo(f).apply(aa)
        }
    )

  def histo[F[_]: Functor, A](f: F[Cofree[F, A]] => A): Fix[F] => A = {
    def toCofree: Fix[F] => Cofree[F, A] =
      fix => Cofree(head = histo(f).apply(fix), tail = fix.unfix map toCofree)

    fix =>
      f(fix.unfix map toCofree)
  }

  def dyna[F[_]: Functor, A, B](f: F[Cofree[F, B]] => B)(g: A => F[A]): A => B = {
    val cofree: F[Cofree[F, B]] => Cofree[F, B] =
      fc => Cofree(f(fc), fc)
    a =>
      hylo(cofree)(g).apply(a).head
  }

  def futu[F[_]: Functor, A](f: A => F[Free[F, A]]): A => Fix[F] = {
    def toFix: Free[F, A] => Fix[F] = {
      case Continue(a) => futu(f).apply(a)
      case Combine(fa) => Fix(fa map toFix)
    }

    a =>
      Fix(f(a) map toFix)
  }

}
