package ex2

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {

  //let's make a Future[Option[A]] flatmappable over A so we can use a for comprehension
  case class FutureOpt[A](val futOpt: Future[Option[A]]) {

    def flatMap[B](f: A => FutureOpt[B]): FutureOpt[B] = FutureOpt(  //  Future[Option[B]]
      futOpt flatMap {
        case Some(a) => f(a).futOpt
        case None => Future.successful(None)
      }
    )

    def map[B](f: A => B): FutureOpt[B] = flatMap { a => FutureOpt(f(a)) }
  }

  object FutureOpt {
    def apply[A](a: A) = new FutureOpt(Future.successful(Some(a)))
  }

  def helloifyUser2(userid: Int): Future[Option[String]] = {

    val result = for {user <- FutureOpt(findUser(userid))
                      helloified <- FutureOpt(helloify(user))
    } yield helloified

    result.futOpt
  }


}
