package chapter4

import cats.MonadError
import cats.implicits._
import org.scalatest.TryValues._
import org.scalatest.EitherValues._

import scala.util.{Success, Try}

object EitherApp extends App {

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]
  }

  assert(validateAdult[Try](18) == Success(18))

  assert(validateAdult[Try](8).failure.exception.getMessage == "Age must be greater than or equal to 18")

  type ExceptionOr[A] = Either[Throwable, A]

  assert(validateAdult[ExceptionOr](-1).left.value.getMessage == "Age must be greater than or equal to 18")
}
