package chapter4

object IdApp extends App {

  trait Monad[F[_]] {
    def pure[A](value: A): F[A]

    def map[A, B](initial: F[A])(func: A => B): F[B]

    def flatMap[A, B](initial: F[A])(func: A => F[B]): Id[B]
  }

  object Monad {
    def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance
  }

  type Id[A] = A

  implicit def idMonad: Monad[Id] =
    new Monad[Id] {
      override def pure[A](value: A): Id[A] = value

      override def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

      // Each type class allows us to sequence operatons ignoring some kind of complicaton.
      // In the case of Id there is no complicaton, making map and flatMap the same thing.
      override def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
    }

  assert(Monad[Id].pure(3) == 3)
  assert(Monad[Id].map(3)(_ * 4) == 12)
  assert(Monad[Id].flatMap(4)(_ * 4) == 16)

  // Notice that we haven’t had to write type annotatons in the method bodies above.
  // The compiler is able to interpret values of type A as Id[A] and vice  versa
  // by the context in which they are used.
}
