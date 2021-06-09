package chapter4

import cats.Monad

object TreeApp extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
  }

  import Tree._

  val tree1: Tree[Int] = branch(branch(leaf(1), leaf(2)), branch(leaf(3), branch(leaf(4), leaf(5))))
  val tree2: Tree[Int] = Monad[Tree].map(tree1)(_ * 3)

  assert(Monad[Tree].flatMap[Int, Int](tree1)(_ => tree2) ==
    Branch(
      Branch(
        Branch(
          Branch(Leaf(3), Leaf(6)),
          Branch(Leaf(9), Branch(Leaf(12), Leaf(15)))
        ),
        Branch(
          Branch(Leaf(3), Leaf(6)),
          Branch(Leaf(9), Branch(Leaf(12), Leaf(15)))
        )
      ),
      Branch(
        Branch(
          Branch(Leaf(3), Leaf(6)),
          Branch(Leaf(9), Branch(Leaf(12), Leaf(15)))
        ),
        Branch(
          Branch(
            Branch(Leaf(3), Leaf(6)),
            Branch(Leaf(9), Branch(Leaf(12), Leaf(15)))
          ),
          Branch(
            Branch(Leaf(3), Leaf(6)),
            Branch(Leaf(9), Branch(Leaf(12), Leaf(15)))
          )
        )
      )
    )
  )

  // The soluton above is perfectly fne for this exercise. Its only downside is tha Cats cannot make guarantees about stack safety.
  // The tail‐recursive soluton however are much harder to write.
  // Refer: https://stackoverflow.com/questions/44504790/cats-non-tail-recursive-tailrecm-method-for-monads
}
