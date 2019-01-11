package algos.recursionscheme

object Example {

  val facF = fix[Int => Int](fac => n => if (n == 0) 1 else n * fac(n - 1))

  def findNeg(l: List[Int]): Option[Int] = l match {
    case Nil    => None
    case x :: l => if (x > 0) findNeg(l) else Some(x)
  }

  val findNegF = fix[List[Int] => Option[Int]](f => {
    case Nil    => None
    case x :: l => if (x > 0) f(l) else Some(x)
  })

}
