package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def identity[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x, xs)
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def head[A](lst: List[A]): A = lst match {
    case Cons(x, xs) => x
  }

  def setHead[A](newHead: A, lst: List[A]): List[A] = {
    if (newHead != Nil) {
      lst match {
        case Nil => Cons(newHead, Nil)
        case Cons(x, xs) => Cons(newHead, xs)
      }
    }
    else Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case n => drop(tail(l), n-1)
  }

  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) { dropWhile(xs, f) }
      else { Cons(x, dropWhile(xs, f)) }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}