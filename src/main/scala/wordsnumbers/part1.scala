package wordsnumbers

import simulacrum._

object part1 {
  @typeclass
  trait Monoid[A] {
    def zero: A
    @op("|+|") def plus(a1: A, a2: A): A
  }

  @typeclass
  trait Seminearring[A] extends Monoid[A] {
    def one: A
    @op("|*|") def times(a1: A, a2: A): A
  }

  @typeclass
  trait Character[A] {
    def char(ch: Char): A
  }

  import Seminearring.ops._

  implicit val idCharacter = new Character[Char] {
    override def char(ch: Char): Char = ch
  }

  implicit val stringCharacter = new Character[String] {
    override def char(ch: Char): String = ch.toString
  }

  implicit def listCharacter[A](implicit C: Character[A]) = new Character[List[A]] {
    override def char(ch: Char): List[A] = List(C.char(ch))
  }

  implicit def stringListSeminearring[A]: Seminearring[List[String]] = new Seminearring[List[String]] {
    override def zero: List[String] = Nil
    override def plus(xs: List[String], ys: List[String]): List[String] = xs ++ ys
    override def one: List[String] = List("")
    override def times(xs: List[String], ys: List[String]): List[String] = for {
      x <- xs
      y <- ys
    } yield x + y
  }

  def product[A](xs: Traversable[A])(implicit S: Seminearring[A]): A = xs.foldRight(S.one)(S.times)

  def string[A](s: String)(implicit S: Seminearring[A], C: Character[A]): A = product[A](s.map(C.char))

  // TODO: Use scalaz/cats/spire
  def sum[A](xs: Traversable[A])(implicit M: Monoid[A]): A = xs.foldRight(M.zero)(M.plus)

  def strings[A](s: String)(implicit S: Seminearring[A], C: Character[A]): A = sum(s.split(' ').map(string[A]))

  def one[A](implicit S: Seminearring[A]) = S.one
  def ten1[A](implicit S: Seminearring[A], C: Character[A]): A = strings[A]("one two three four five six seven eight nine")
  def ten2[A](implicit S: Seminearring[A], C: Character[A]): A = {
    val prefixes = strings[A]("fif six seven eigh nine")
    ten1[A] |+| strings[A]("ten eleven twelve") |+|
      ((strings[A]("thir four") |+| prefixes) |*| string[A]("teen")) |+|
      ((strings[A]("twen thir for") |+| prefixes) |*| string[A]("ty") |*| (one[A] |+| ten1[A]))
  }
  def ten3[A](implicit S: Seminearring[A], C: Character[A]): A = {
    ten2[A] |+| (ten1[A] |*| string[A]("hundred") |*| (one[A] |+| ten2[A]))
  }
  def ten6[A](implicit S: Seminearring[A], C: Character[A]): A = ten3[A] |+| (ten3[A] |*| string[A]("thousand") |*| (one[A] |+| ten3[A]))
  def ten9[A](implicit S: Seminearring[A], C: Character[A]): A = ten6[A] |+| (ten3[A] |*| string[A]("million") |*| (one[A] |+| ten6[A]))
}
