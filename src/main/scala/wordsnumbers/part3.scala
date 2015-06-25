package wordsnumbers

import part1._

object part3 {
  case class Binary[M](n: M, d: Option[(Binary[M],Binary[M])])

  implicit def binaryCharacter[M](implicit C: Character[M]): Character[Binary[M]] = new Character[Binary[M]]() {
    override def char(ch: Char): Binary[M] = Binary(C.char(ch), None)
  }

  implicit def binaryMonoid[M](implicit M: Monoid[M]): Monoid[Binary[M]] = new Monoid[Binary[M]] {
    override def zero: Binary[M] = Binary(M.zero, None)
    override def plus(b1: Binary[M], b2: Binary[M]): Binary[M] = Binary(M.plus(b1.n,b2.n), Some((b1,b2)))
  }
}
