package chapter3

import cats.Functor

object TreeApp extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }
  }

  /**
   * val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
   * tree.map(_ * 2)
   *
   * Results in compile time exception: value map is not a member of chapter3.TreeApp.Branch[Int]
   * tree.map(_ * 2)
   *
   * This is due to the fact that cats type classes are invarient. It requires us to specify
   * instances for subtypes explicitly instead of using super type class instance.
   *
   * Refer: https://www.notion.so/Scala-with-Cats-d12546468fc64385a7fc75c4cee00c80#bf1ed3970b2f4d238b56d20adfa351bc
   */

  import Tree._
  import cats.implicits.toFunctorOps

  val tree = branch(branch(leaf(1), leaf(2)), branch(leaf(3), branch(leaf(4), leaf(5))))
  assert(tree.map(_ * 2) == Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Branch(Leaf(8), Leaf(10)))))
}
