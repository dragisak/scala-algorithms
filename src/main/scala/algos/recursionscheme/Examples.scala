package algos.recursionscheme

object Examples {

  def findNeg(l: List[Int]): Option[Int] = l match {
    case Nil    => None
    case x :: l => if (x > 0) findNeg(l) else Some(x)
  }

  val findNegF = fix[List[Int] => Option[Int]](f => {
    case Nil    => None
    case x :: l => if (x > 0) f(l) else Some(x)
  })

}

object Factorial {

  import Stack._

  val facFix = fix[Int => Int](fac => n => if (n == 0) 1 else n * fac(n - 1))

  val facStackCoalgebra: Int => Stack[Int] = n => if (n > 0) more(n - 1, n) else done()

}
