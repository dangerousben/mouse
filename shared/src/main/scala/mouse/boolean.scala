package mouse

import cats.{ Applicative, Monoid }
import alleycats.Empty

trait BooleanSyntax {
  implicit final def booleanSyntaxMouse(b: Boolean): BooleanOps = new BooleanOps(b)
}

final class BooleanOps(val b: Boolean) extends AnyVal {

  @inline def option[A](a: => A): Option[A] = fold(Some(a), None)

  @deprecated("Use `either` instead", "0.6")
  @inline def xor[L, R](l: =>L, r: =>R): Either[L, R] = either(l, r)

  @inline def either[L, R](l: =>L, r: =>R): Either[L, R] = fold(Right(r), Left(l))

  @inline def fold[A](t: => A, f: => A): A = if (b) t else f

  @inline def valueOrZero[A](a: => A)(implicit M: Monoid[A]): A = if (b) a else M.empty

  @inline def zeroOrValue[A](a: => A)(implicit M: Monoid[A]): A = if (b) M.empty else a

  @inline def ??[A](a: => A)(implicit M: Monoid[A]): A = valueOrZero(a)

  @inline def !?[A](a: => A)(implicit M: Monoid[A]): A = zeroOrValue(a)

  @inline def valueOrEmpty[A](a: => A)(implicit E: Empty[A]): A = if (b) a else E.empty

  @inline def emptyOrValue[A](a: => A)(implicit E: Empty[A]): A = if (b) E.empty else a

  @inline def valueOrPure[F[_], A](fa: =>F[A])(a: =>A)(implicit F: Applicative[F]) = if (b) fa else F.pure(a)
}
