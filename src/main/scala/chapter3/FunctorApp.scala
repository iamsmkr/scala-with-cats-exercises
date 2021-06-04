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

  final case class Box[A](value: A)

  def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = ContravariantFunctor[Printable].contramap[A, Box[A]](p)(_.value)

  implicit val stringPrintable: Printable[String] = value => s"'$value'"

  println(boxPrintable[String].format(Box("helloworld")))
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

  val integerCodec: Codec[Int] = InvariantFunctor[Codec].imap(stringCodec)(_.toInt)(_.toString)

  println(integerCodec.encode(2))
  println(integerCodec.decode("5"))

  final case class Box[T](value: T)

  val boxCodec: Codec[Box[String]] = InvariantFunctor[Codec].imap(stringCodec)(Box(_))((x: Box[String]) => x.value)

  println(boxCodec.encode(Box("helloworld")))
  println(boxCodec.decode("helloworld"))

  trait Monoid[T] {
    def empty: T

    def combine(x: T, y: T): T
  }

  // Since both the methods `empty` and `combine` in a Monoind are varying in both parameter as well as return type
  // we have to provide transformations both from `A=>B` and `B=>A` to be able to convert a `F[A]` to `F[B]`.
  // This, however, also means that we would be convert `F[B]` to `F[A]`.

  implicit def monoidInvariantFunctor: InvariantFunctor[Monoid] =
    new InvariantFunctor[Monoid] {
      override def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] =
        new Monoid[B] {
          override def empty: B = f(fa.empty)

          override def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        }
    }

  implicit val stringMonoid: Monoid[String] =
    new Monoid[String] {
      override def empty: String = ""

      override def combine(x: String, y: String): String = x + y
    }

  implicit def symbolMonoid(implicit m: Monoid[String]): Monoid[Symbol] = InvariantFunctor[Monoid].imap[String, Symbol](m)(Symbol.apply)(_.name)

  println(symbolMonoid.empty)
  println(symbolMonoid.combine(Symbol("A"), Symbol("B")))

  // Although the implicit monoid `Monoid[Symbol]` in turns makes use of `stringMonoid` but the following implementation
  // is enough to put the point across: that since both the methods `empty` and `combine` in a Monoind are varying in both
  // parameter as well as return type, we can derive both `F[A]` from `F[B]` as well as `F[B]` from `F[A]`.

  def stringMonoid2(implicit m: Monoid[Symbol]): Monoid[String] = InvariantFunctor[Monoid].imap[Symbol, String](m)(_.name)(Symbol.apply)

  println(stringMonoid2.empty)
  println(stringMonoid2.combine("X", "Y"))
}
