package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B):Option[B] = this match {
    case None => None
    case Some(a)=> Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /*
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  */
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

}

object options {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def div(x: Int, y: Int): Option[Int] =
    if (y == 0) None
    else Some(x/y)
}

object tester {
  def run() = {
    val f = (x: Int) => x.toDouble + 2.0
    val opt = Some(1)
    opt.map(f)
  }
}