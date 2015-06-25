package wordsnumbers

import part1._
import Seminearring.ops._

case class Count(n: Long) extends AnyVal

case class Wrap[S,A](a: A) extends AnyVal

object part2 {
  trait Module[R,M] {
    def timesl(r: R, m: M): M = timesr(m, r)
    def timesr(m: M, r: R): M = timesl(r, m)
  }

  case class Deriv[R, M](r: R, m: M)

  implicit def countCharacter = new Character[Count] {
    override def char(ch: Char): Count = Count(1)
  }

  implicit def countSeminearring = new Seminearring[Count] {
    override def zero: Count = Count(0)
    override def plus(a1: Count, a2: Count): Count = Count(a1.n + a2.n)
    override def one: Count = Count(1)
    override def times(a1: Count, a2: Count): Count = Count(a1.n * a2.n)
  }

  // TODO: Get operators working
  implicit def derivSeminearring[R,M](implicit R: Seminearring[R], M: Monoid[M], V: Module[R,M]) = new Seminearring[Deriv[R,M]] {
    override def zero: Deriv[R, M] = Deriv(R.zero, M.zero)
    override def plus(d1: Deriv[R, M], d2: Deriv[R, M]): Deriv[R, M] = (d1, d2) match {
      case (Deriv(c1, m1), Deriv(c2, m2)) => Deriv(c1 |+| c2, M.plus(m1, m2))
    }
    override def one: Deriv[R, M] = Deriv(R.one, M.zero)
    override def times(d1: Deriv[R, M], d2: Deriv[R, M]): Deriv[R, M] = (d1, d2) match {
      case (Deriv(c1, m1), Deriv(c2, m2)) => Deriv(R.times(c1, c2), M.plus(V.timesl(c1, m2), V.timesr(m1, c2)))
    }
  }

  implicit def derivCharacter[R,M](implicit CR: Character[R], CM: Character[M]): Character[Deriv[R,M]] = new Character[Deriv[R, M]] {
    override def char(ch: Char): Deriv[R, M] = Deriv(CR.char(ch), CM.char(ch))
  }

  import Monoid.ops._
  implicit def wrapMonoid[S,M](implicit M: Monoid[M]): Monoid[Wrap[S, M]] = new Monoid[Wrap[S,M]] {
    override def zero: Wrap[S, M] = Wrap(M.zero)
    override def plus(w1: Wrap[S, M], w2: Wrap[S, M]): Wrap[S, M] = Wrap(w1.a |+| w2.a)
  }

  implicit def wrapSeminearing[S,R](implicit R: Seminearring[R]): Seminearring[Wrap[S, R]] = new Seminearring[Wrap[S, R]] {
    override def zero: Wrap[S, R] = Wrap(R.zero)
    override def plus(w1: Wrap[S, R], w2: Wrap[S, R]): Wrap[S, R] = Wrap(R.plus(w1.a, w2.a))
    override def one: Wrap[S, R] = Wrap(R.one)
    override def times(w1: Wrap[S, R], w2: Wrap[S, R]): Wrap[S, R] = Wrap(R.times(w1.a, w2.a))
  }

  implicit def wrapModule[S,R](implicit R: Seminearring[R]): Module[R,Wrap[S,R]] = new Module[R, Wrap[S, R]] {
    override def timesl(r: R, wm: Wrap[S, R]): Wrap[S, R] = Wrap(R.times(r,wm.a))
    override def timesr(wm: Wrap[S, R], r: R): Wrap[S, R] = Wrap(R.times(wm.a,r))
  }

  trait V
  type Volume = Wrap[V, Count]

  implicit val volumeCharacter = new Character[Volume] {
    override def char(ch: Char): Volume = implicitly[Seminearring[Volume]].one
  }
}
