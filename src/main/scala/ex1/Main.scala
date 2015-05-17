package ex1

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {

  //let's offer to our clients a combined function that looks up the user and returns the greeting
  def helloifyUser(userid: Int): Future[Option[String]] = {

    findUser(userid) flatMap {
      case Some(user) => helloify(user)
      case None => Future.successful(None)
    }
  }

}
