package shared

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Helloify {

  case class User(name: String, id: Int)

  //a spiffy asynchronous api for saying hello to our users
  val users = List(User("piet", 1))

  def findUser(userid: Int): Future[Option[User]] = Future(users.find(_.id == userid))

  def helloify(user: User): Future[Option[String]] = Future(Some("hello " + user.name))


}

