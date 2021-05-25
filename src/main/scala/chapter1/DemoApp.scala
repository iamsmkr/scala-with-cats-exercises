package chapter1

object DemoApp extends App {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      override def format(cat: Cat): String = {
        import PrintableInstances._

        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"$name is a $age year-old $color cat."
      }
    }

  val cat = Cat("Molly", 5, "Black")

  Printable.print(cat)

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

  println(showCat.show(cat))
  println(cat.show)
  println(implicitly[Show[Cat]].show(cat))

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  import cats.implicits._

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) && (cat2.age === cat2.age) &&
      (cat2.color === cat2.color)
  }

  println(cat1 === cat2)
  println(cat1 =!= cat2)
  println(Option(cat1) === Option.empty[Cat])
  println(cat1.some =!= none[Cat])
  println(cat1.some === cat2.some)
}
