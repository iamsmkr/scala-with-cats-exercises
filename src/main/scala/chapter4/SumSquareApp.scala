package chapter4

import cats.Monad
import cats.implicits._

object SumSquareApp extends App {

  val res = List(1, 2, 3).flatMap(x => List(4, 5).map(y => x * x + y * y))
  println(res)

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1, 2, 3), List(4, 5)))

  def sumSquare2[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare2(Option(3), Option(4)))
  println(sumSquare2(List(1, 2, 3), List(4, 5)))
}
