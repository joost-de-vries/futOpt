package shared

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Helloify {

  case class User(name: String, id: Int, friendly:Boolean)

  //a spiffy asynchronous api for saying hello to our users
  val users = List(User(name="piet", id= 1,friendly=true))

  def findUser(userid: Int): Future[Option[User]] = Future(
    users.find(_.id == userid)
  )

  def helloify(user: User): Future[Option[String]] = Future(
    if (user.friendly) Some("hello " + user.name) else None
  )
}

