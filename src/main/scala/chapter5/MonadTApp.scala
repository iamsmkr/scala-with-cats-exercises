package chapter5

import cats.{Functor, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MonadTApp extends App {

  case class User(fullName: String)

  case class Address(state: String, country: String)

  val users: Map[Long, User] =
    Map(
      1L -> User("Shivam Kapoor"),
      2L -> User("Naman Gupta")
    )

  val addresses: Map[User, Address] =
    Map(
      User("Shivam Kapoor") -> Address("Bangalore", "India"),
      User("Naman Gupta") -> Address("Colombo", "Sri Lanka")
    )

  def showResults[A](f: () => Future[A]): Unit = println(Await.result(f(), 1.seconds))

  def findUserById(id: Long): Future[Option[User]] = Future(users.get(id))

  def findAddressByUser(user: User): Future[Option[Address]] = Future(addresses.get(user))

  def findAddressByUserId(id: Long): Future[Option[Address]] =
    findUserById(id).flatMap(_.fold(Future.successful[Option[Address]](None))(findAddressByUser))

  showResults(() => findAddressByUserId(1L))

  def findAddressByUserId2(id: Long): Future[Option[Address]] = {
    val f = for {
      maybeUser <- findUserById(id)
    } yield maybeUser.fold(Future.successful[Option[Address]](None))(findAddressByUser)

    f.flatMap(identity)
  }

  showResults(() => findAddressByUserId2(1L))

  case class OptionT[F[_], A](value: F[Option[A]]) {
    def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
      OptionT(F.map(value)(_.map(f)))

    def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
      flatMapF(a => f(a).value)

    def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] = {
      OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))
    }
  }

  def findAddressByUserId3(id: Long): Future[Option[Address]] = {
    val f = for {
      user <- OptionT(findUserById(id))
      address <- OptionT(findAddressByUser(user))
    } yield address

    f.value
  }

  showResults(() => findAddressByUserId3(1L))

  def findAddressByUserId4(id: Long): Future[Option[Address]] =
    OptionT(findUserById(id)).flatMap(user => OptionT(findAddressByUser(user))).value

  showResults(() => findAddressByUserId4(1L))

}
