sealed trait List2[+A] {
  def getHead = this match {
    case Cons(h, t) => h
  }
}
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

