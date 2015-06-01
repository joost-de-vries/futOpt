package ex1

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {

  def helloifyUser(userid: Int): Future[Option[String]] = {

    findUser(userid) flatMap {
      case Some(user) => helloify(user)
      case None => Future.successful(None)
    }
  }

}
