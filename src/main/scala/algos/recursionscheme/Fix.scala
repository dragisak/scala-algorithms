package algos.recursionscheme

final case class Fix[F[_]](unfix: F[Fix[F]])
