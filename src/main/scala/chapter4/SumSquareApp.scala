package chapter4

import cats.{Id, Monad}
import cats.implicits._

object SumSquareApp extends App {

  val res = List(1, 2, 3).flatMap(x => List(4, 5).map(y => x * x + y * y))
  assert(res == List(17, 26, 20, 29, 25, 34))

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  assert(sumSquare(Option(3), Option(4)).contains(25))
  assert(sumSquare(List(1, 2, 3), List(4, 5)) == List(17, 26, 20, 29, 25, 34))

  def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  assert(sumSquare2(Option(3), Option(4)).contains(25))
  assert(sumSquare2(List(1, 2, 3), List(4, 5)) == List(17, 26, 20, 29, 25, 34))

  assert(sumSquare(4: Id[Int], 6: Id[Int]) == 52)
}
