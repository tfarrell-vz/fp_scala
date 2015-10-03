sealed trait BinaryTree[+A]
case object Nil extends BinaryTree[Nothing]
case class Node[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  
}