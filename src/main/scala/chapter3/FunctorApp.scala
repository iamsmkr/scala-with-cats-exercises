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

  assert(func3(2) == 2.0)

  import cats.syntax.functor._

  val func4: Function[Int, Double] = func1.map(func2)

  assert(func4(2) == 2.0)
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

  assert(integerPrintable.format(2) == "'2'")

  def func: Double => Int = _.toInt

  val doublePrintable: Printable[Double] = ContravariantFunctor[Printable].contramap(integerPrintable)(func)

  assert(doublePrintable.format(2.0) == "'2'")

  final case class Box[A](value: A)

  def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = ContravariantFunctor[Printable].contramap[A, Box[A]](p)(_.value)

  implicit val stringPrintable: Printable[String] = value => s"'$value'"

  assert(boxPrintable[String].format(Box("helloworld")) == "'helloworld'")
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

  implicit val integerCodec: Codec[Int] = InvariantFunctor[Codec].imap(stringCodec)(_.toInt)(_.toString)

  assert(integerCodec.encode(2) == "2")
  assert(integerCodec.decode("5") == 5)

  val stringCodec2: Codec[String] = InvariantFunctor[Codec].imap(integerCodec)(_.toString)(_.toInt)

  final case class Box[T](value: T)

  val boxCodec: Codec[Box[String]] = InvariantFunctor[Codec].imap(stringCodec)(Box(_))((x: Box[String]) => x.value)

  assert(boxCodec.encode(Box("helloworld")) == "helloworld")
  assert(boxCodec.decode("helloworld") == Box("helloworld"))

  trait Monoid[T] {
    def empty: T

    def combine(x: T, y: T): T
  }

  // Since both the methods `empty` and `combine` in a Monoid are varying in both parameter as well as return type
  // we have to provide transformations both from `A=>B` and `B=>A` to be able to convert a `F[A]` to `F[B]`.
  // This also means that we would be convert `F[B]` to `F[A]`. However, this is possible to all invariant functors.

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

  assert(symbolMonoid.empty == Symbol(""))
  assert(symbolMonoid.combine(Symbol("A"), Symbol("B")) == Symbol("AB"))

  def stringMonoid2(implicit m: Monoid[Symbol]): Monoid[String] = InvariantFunctor[Monoid].imap[Symbol, String](m)(_.name)(Symbol.apply)

  assert(stringMonoid2.empty == "")
  assert(stringMonoid2.combine("X", "Y") == "XY")
}
