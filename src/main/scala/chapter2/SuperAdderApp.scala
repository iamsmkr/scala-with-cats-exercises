package chapter2

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup

object SuperAdderApp extends App {
  // -- Part 1 --
  def add1(items: List[Int]): Int = items.sum

  assert(add1(List(1, 2, 3)) == 6)


  // -- Part 2 --
  def add2(items: List[Option[Int]]): Option[Int] =
    Option(
      items.foldLeft(0) { (acc, item) =>
        item.map(acc + _).getOrElse(acc)
      }
    )

  assert(add2(List(Some(1), None, Some(3))).contains(4))


  // -- Part 3 --
  case class Order(totalCost: Double, quantity: Double)

  def add3(orders: List[Order]): Order =
    orders.foldLeft(Order(0.0, 0.0)) { (acc, order) =>
      Order(acc.totalCost + order.totalCost, acc.quantity + order.quantity)
    }

  assert(add3(List(Order(1.0, 2.0), Order(3.0, 4.0))) == Order(4.0, 6.0))


  // -- Using Monoids --

  def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

  assert(add(List(1, 2, 3)) == 6)

  assert(add(List(Some(1), None, Some(3))).contains(4))
  assert(add(List(Option(1), Option(2), Option(3))).contains(6))

  /**
   * If we try to add a list consistng entirely of Some values, we get a compile error:
   *
   * add21(List(Some(1), Some(2), Some(3)).contains(6))) results in compile error
   * error: could not find implicit value for evidence parameter of type cats.Monoid[Some[Int]]
   * add21(List(Some(1), Some(2), Some(3)))
   * ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   *
   * This error occurs because of Cats decision of implementing type classes as invarients.
   * In this particular case, despite Some being subclass of Option, type class instance of Option
   * is not used for Some type. We can solve this problem with a type annotation like Some(1): Option[Int]
   * or by using "smart constructors" like the Option.apply, Option.empty, some and none methods.
   */

  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      override def empty: Order = Order(0.0, 0.0)

      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }

  assert(add(List(Order(1.0, 2.0), Order(3.0, 4.0))) == Order(4.0, 6.0))
}
