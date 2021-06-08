package chapter4

import cats.data.Writer
import cats.implicits._

object WriterApp extends App {

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds)

  type Logged[A] = Writer[Vector[String], A]

  def factorial2(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial2(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val res = Await.result(Future.sequence(Vector(
    Future(factorial2(5)),
    Future(factorial2(5))
  )).map(_.map(_.written)), 5.seconds)

  assert(res == Vector(
    Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"),
    Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120")
  ))
}
