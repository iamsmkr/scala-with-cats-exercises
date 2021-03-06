package chapter1

object DemoApp extends App {

  final case class Cat(name: String, age: Int, color: String)

  // value object could be implemented as implicit objects
  implicit object PrintableCat extends Printable[Cat] {
    override def format(cat: Cat): String = {
      import PrintableInstances._

      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }

  val cat = Cat("Molly", 5, "Black")

  implicitly[Printable[Cat]].format(cat)

  Printable.print(cat)

  import PrintableCat._

  cat.format

  import chapter1.PrintableSyntax._

  cat.print

  import cats._
  import cats.syntax.show._

  implicit val showCat: Show[Cat] =
    Show.show[Cat] { cat =>
      val name = Show[String].show(cat.name)
      val age = Show[Int].show(cat.age)
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }

  assert(showCat.show(cat) == "Molly is a 5 year-old Black cat.")
  assert(cat.show == "Molly is a 5 year-old Black cat.")
  assert(implicitly[Show[Cat]].show(cat) == "Molly is a 5 year-old Black cat.")
  assert(Show[Cat].show(cat) == "Molly is a 5 year-old Black cat.")

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  import cats.implicits._

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) && (cat2.age === cat2.age) &&
      (cat2.color === cat2.color)
  }

  assert(!(cat1 === cat2))
  assert(cat1 =!= cat2)
  assert(!(Option(cat1) === Option.empty[Cat]))
  assert(cat1.some =!= none[Cat])
  assert(!(cat1.some === cat2.some))
}
