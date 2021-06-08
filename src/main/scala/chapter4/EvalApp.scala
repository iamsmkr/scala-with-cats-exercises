package chapter4

import cats.Eval

object EvalApp extends App {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  // foldRight((1 to 100000).toList, 0L)(_ + _)
  // Stack overflows

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight1[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value
  }

  assert(foldRight1((1 to 100000).toList, 0L)(_ + _) == 5000050000L)
}
