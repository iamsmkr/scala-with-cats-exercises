package chapter5

import scala.util.Try

object DemoApp extends App {

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed to read $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val res = for {
      x <- OptionT(parseNumber(a))
      y <- OptionT(parseNumber(b))
      z <- OptionT(parseNumber(c))
    } yield x + y + z

    res.value
  }

  assert(addAll("1", "2", "3") == Writer(List("Read 1", "Read 2", "Read 3"), Some(6)))
  assert(addAll("1", "a", "3") == Writer(List("Read 1", "Failed to read a"), None))

}
