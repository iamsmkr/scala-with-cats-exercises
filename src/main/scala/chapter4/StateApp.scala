package chapter4

import cats.data.State
import cats.implicits._

object StateApp extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    def operand(num: Int): CalcState[Int] =
      State[List[Int], Int] { stack =>
        (num :: stack, num)
      }

    def operator(f: (Int, Int) => Int): CalcState[Int] =
      State[List[Int], Int] {
        case a :: b :: tail =>
          val ans = f(a, b)
          (ans :: tail, ans)

        case _ => sys.error("Fail!")
      }

    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  assert(evalInput("1 2 + 3 4 + *") == 21)
}
