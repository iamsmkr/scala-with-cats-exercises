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
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  val res1 = branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

  assert(res1 ==
    Branch(
      Branch(Leaf(99), Leaf(101)),
      Branch(Leaf(199), Leaf(201))
    )
  )

  val res2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  assert(res2 ==
    Branch(
      Branch(
        Branch(Leaf(89), Leaf(91)),
        Branch(Leaf(109), Leaf(111))
      ),
      Branch(
        Branch(Leaf(189), Leaf(191)),
        Branch(Leaf(209), Leaf(211))
      )
    )
  )

  // The soluton above is perfectly fne for this exercise. Its only downside is tha Cats cannot make guarantees about stack safety.
  // The tail‐recursive soluton however are much harder to write.
  // Refer: https://stackoverflow.com/questions/44504790/cats-non-tail-recursive-tailrecm-method-for-monads
}
