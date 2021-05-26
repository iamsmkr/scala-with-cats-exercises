package chapter1

trait Printable[A] {
  def format(value: A): String

  // interface syntax could be implemented as part of the type class interface in favour of Rich Object pattern
  implicit class RichInterface(value: A) {
    def format(implicit valuePrintable: Printable[A]): String =
      valuePrintable.format(value)
  }
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = value
    }

  // single abstract method syntax sugar
  implicit val intPrintable: Printable[Int] = _.toString
}

object Printable {

  // we can use context bound syntax sugar
  def format[A: Printable](value: A): String = {
    val valuePrintable = implicitly[Printable[A]]
    valuePrintable.format(value)
  }

  def print[A: Printable](value: A)(implicit valuePrintable: Printable[A]): Unit =
    println(valuePrintable.format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def print(implicit valuePrintable: Printable[A]): Unit =
      println(valuePrintable.format(value))
  }

}
