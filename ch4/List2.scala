sealed trait List2[+A] {
  def head[A]: A = this match {
    case Cons(h, t) => h
  }
}
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List2[A]

