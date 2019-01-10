package algos.recursionscheme

object Example {

  val facF = fix[Int => Int](fac => n => if (n == 0) 1 else n * fac(n - 1))

}
