package ex4

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {
  //let's create our own monad transformer for option
  // looking at our implementation of FutureOption we need a way to flatmap and to instantiate
  //scala doesn't have a trait Monad

  import scala.language.higherKinds

  type FlatMapFunc[F[_],A,B] = (A => F[B]) => F[B]

  case class OptionT[F[_], A](opt: F[Option[A]]) {

    def flatMap[B](f: A => OptionT[F, B])
                  (implicit flatmappify: F[Option[A]] => FlatMapFunc[F,Option[A],B],
                   instantiate: Option[B] => F[Option[B]]): OptionT[F, B] = OptionT[F, B]({
      val r=flatmappify(opt){ aOpt=>
        val r2:F[Option[B]]=aOpt match {
          case Some(a) => f(a).opt
          case None => instantiate(None:Option[B])
        }
        ???
      }
      ???
    }
    )

    def map[B](f: A => B)(implicit flatmappify: F[Option[A]] => FlatMapFunc[F,Option[A],B],
                          instantiate: Option[B] => F[Option[B]]): OptionT[F, B] = flatMap { a => OptionT[F, B](instantiate(Some(f(a)))) }
  }

  object OptionT {
  }

//  def helloifyUser4(userid: Int): Future[Option[String]] = {
//    implicit val unit: Option[String] => Future[Option[String]] = Future.successful _
//    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f(_))
//
//    implicit val f = flatMap _
//
//    val result = for {
//      a <- OptionT(findUser(userid))
//      ab <- OptionT(helloify(a))
//    } yield ab
//    result.opt
//  }


}
