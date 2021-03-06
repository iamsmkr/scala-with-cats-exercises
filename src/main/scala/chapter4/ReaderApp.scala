package chapter4

import cats.data.Reader
import cats.syntax.applicative._

object ReaderApp extends App {

  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUsername(userId)
      passwordOk <- maybeUsername.map(username => checkPassword(username, password))
        .getOrElse(false.pure[DbReader])
    } yield passwordOk

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

  val db = Db(users, passwords)

  assert(checkLogin(1, "zerocool").run(db))
  assert(!checkLogin(4, "davinci").run(db))

}
