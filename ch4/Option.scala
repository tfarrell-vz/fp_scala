package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  /*
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  */
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map[A, B](opt: Option[A], f: A => B): Option[B] = opt match {
    case None => None
    case Some(a) => Some(f(a))
  }
  
}

object options {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}