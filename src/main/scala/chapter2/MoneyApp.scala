package chapter2

object MoneyApp extends App {

  case class Money(dollars: Int, cents: Int)

  def addMoney(money1: Money, money2: Money): Money = {
    val totalCents = (money1.cents + money2.cents) % 100
    val totalDollars = money1.dollars + money2.dollars + (money1.cents + money2.cents) / 100

    Money(totalDollars, totalCents)
  }

  assert(addMoney(Money(500, 45), Money(330, 90)) == Money(831, 35))

  def addMoneyMap(balances: Map[String, Money], salaries: Map[String, Money]): Map[String, Money] = {
    balances.foldLeft(salaries) { case (acc, (emp, money)) =>
      acc + (emp -> acc.get(emp).map(addMoney(_, money)).getOrElse(money))
    }
  }

  assert(addMoneyMap(
    balances = Map("James" -> Money(212, 98), "Jimmy" -> Money(43, 44)),
    salaries = Map("Jimmy" -> Money(500, 44))
  ) == Map("Jimmy" -> Money(543, 88), "James" -> Money(212, 98)))

}

object SemigroupMoneyApp extends App {

  case class Money(dollars: Int, cents: Int)

  trait Addable[A] {
    def add(a1: A, a2: A): A
  }

  def add[A: Addable](a1: A, a2: A): A = implicitly[Addable[A]].add(a1, a2)

  implicit val addableMoney: Addable[Money] = (money1, money2) => {
    val totalCents = (money1.cents + money2.cents) % 100
    val totalDollars = money1.dollars + money2.dollars + (money1.cents + money2.cents) / 100

    Money(totalDollars, totalCents)
  }

  implicit def addableMap[K, V: Addable]: Addable[Map[K, V]] = (m1, m2) => {
    m1.foldLeft(m2) { case (acc, (x, y)) =>
      acc + (x -> acc.get(x).map(implicitly[Addable[V]].add(_, y)).getOrElse(y))
    }
  }

  assert(add(Money(500, 45), Money(330, 90)) == Money(831, 35))

  assert(add(
    Map("James" -> Money(212, 98), "Jimmy" -> Money(43, 44)),
    Map("Jimmy" -> Money(500, 44))
  ) == Map("Jimmy" -> Money(543, 88), "James" -> Money(212, 98)))

}

object CatsSemigroupMoneyApp extends App {

  import cats._

  case class Money(dollars: Int, cents: Int)

  implicit val semigroupMoney: Semigroup[Money] = (money1, money2) => {
    val totalCents = (money1.cents + money2.cents) % 100
    val totalDollars = money1.dollars + money2.dollars + (money1.cents + money2.cents) / 100

    Money(totalDollars, totalCents)
  }

  def add[A: Semigroup](a1: A, a2: A): A = implicitly[Semigroup[A]].combine(a1, a2)

  assert(add(Money(500, 45), Money(330, 90)) == Money(831, 35))

  assert(add(
    Map("James" -> Money(212, 98), "Jimmy" -> Money(43, 44)),
    Map("Jimmy" -> Money(500, 44))
  ) == Map("Jimmy" -> Money(543, 88), "James" -> Money(212, 98)))

  import cats.syntax.semigroup._

  assert((
    Map("James" -> Money(212, 98), "Jimmy" -> Money(43, 44))
      |+| Map("Jimmy" -> Money(500, 44))
    ) == Map("Jimmy" -> Money(543, 88), "James" -> Money(212, 98)))
}
