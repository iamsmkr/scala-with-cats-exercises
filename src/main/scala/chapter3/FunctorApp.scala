package chapter3

object FunctorApp extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]](implicit instance: Functor[F]): Functor[F] = instance
  }

  type MyFunc[T] = Function[Int, T]

  implicit def functionFunctor: Functor[MyFunc] =
    new Functor[MyFunc] {
      override def map[A, B](fa: MyFunc[A])(f: A => B): MyFunc[B] = f.compose(fa)
    }

  val func1 = new MyFunc[String] {
    override def apply(v1: Int): String = v1.toString
  }

  val func2: String => Double = _.toDouble

  val func3: MyFunc[Double] = Functor[MyFunc].map(func1)(func2)

  println(func3(2)) // returns 2.0

  import cats.syntax.functor._

  val func4: Function[Int, Double] = func1.map(func2)

  println(func4(2)) // returns 2.0
}

object ContravariantFunctorApp extends App {

  trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  object ContravariantFunctor {
    def apply[F[_]](implicit instance: ContravariantFunctor[F]): ContravariantFunctor[F] = instance
  }

  trait Printable[T] {
    def format(value: T): String
  }

  implicit def printableContravariantFunctor: ContravariantFunctor[Printable] =
    new ContravariantFunctor[Printable] {
      override def contramap[A, B](fa: Printable[A])(f: B => A): Printable[B] =
        new Printable[B] {
          override def format(value: B): String = fa.format(f(value))
        }
    }

  implicit val integerPrintable: Printable[Int] = value => s"'$value'"

  println(integerPrintable.format(2))

  def func: Double => Int = _.toInt

  val doublePrintable: Printable[Double] = ContravariantFunctor[Printable].contramap(integerPrintable)(func)

  println(doublePrintable.format(2.0))
}

object InvariantFunctorApp extends App {

  trait InvariantFunctor[F[_]] {
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  }

  object InvariantFunctor {
    def apply[F[_]](implicit instance: InvariantFunctor[F]): InvariantFunctor[F] = instance
  }

  trait Codec[T] {
    def encode(value: T): String

    def decode(value: String): T
  }

  implicit def codecInvariantFunctor: InvariantFunctor[Codec] =
    new InvariantFunctor[Codec] {
      override def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A): Codec[B] =
        new Codec[B] {
          override def encode(value: B): String = fa.encode(g(value))

          override def decode(value: String): B = f(fa.decode(value))
        }
    }

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      override def encode(value: String): String = value

      override def decode(value: String): String = value
    }

  val integerCodec: Codec[Int] = InvariantFunctor[Codec].imap[String, Int](stringCodec)(_.toInt)(_.toString)

  println(integerCodec.encode(2))
  println(integerCodec.decode("5"))
}
