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

  val showCat = Show.show((cat: Cat) => {
    val name = Show[String].show(cat.name)
    val age = Show[Int].show(cat.age)
    val color = Show[String].show(cat.color)
    s"$name is a $age year-old $color cat."
  })

  println(showCat.show(cat))
}
