package chapter1

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = value
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      override def format(value: Int): String = value.toString
    }
}

object Printable {
  def format[A](value: A)(implicit valuePrintable: Printable[A]): String =
    valuePrintable.format(value)

  def print[A](value: A)(implicit valuePrintable: Printable[A]): Unit =
    println(valuePrintable.format(value))
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit valuePrintable: Printable[A]): String =
      valuePrintable.format(value)

    def print(implicit valuePrintable: Printable[A]): Unit =
      println(valuePrintable.format(value))
  }

}
