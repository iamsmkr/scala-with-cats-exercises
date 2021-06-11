package chapter5

import cats.Id
import cats.data._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object AutobotsApp extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    val powerLevel = powerLevels.get(autobot)
    EitherT(Future(Either.cond(powerLevel.isDefined, powerLevel.get, s"$autobot unreachable")))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

  assert(tacticalReport("Jazz", "Bumblebee") == "Jazz and Bumblebee need a recharge.")
  assert(tacticalReport("Bumblebee", "Hot Rod") == "Bumblebee and Hot Rod are ready to roll out!")
  assert(tacticalReport("Jazz", "Ironhide") == "Comms error: Ironhide unreachable")
}
