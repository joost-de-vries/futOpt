package ex5

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {
  //let's create our own monad transformer for option
  // looking at our implementation of FutureOption we need a way to flatmap and to instantiate
  //scala doesn't have a trait Monad

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def apply[A](a: A): F[A]
  }

  case class OptionT[F[_], A](opt: F[Option[A]]) {

    def flatMap[B](f: A => OptionT[F, B])(implicit FF: Monad[F]): OptionT[F, B] = OptionT[F, B](
      FF.flatMap(opt) {
        case Some(a) => f(a).opt
        case None => FF.apply(None: Option[B])
      }
    )

    def map[B](f: A => B)(implicit FF: Monad[F]): OptionT[F, B] = flatMap { a => OptionT[F, B](FF.apply(Some(f(a)))) }
  }

  object OptionT {
    implicit val monad = new Monad[Future] {
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f(_))

      def apply[A](a: A): Future[A] = Future.successful(a)
    }
  }

  def helloifyUser4(userid: Int): Future[Option[String]] = {
    implicit val unit: Option[String] => Future[Option[String]] = Future.successful _

    import OptionT.monad
    val result = for {
      a <- OptionT(findUser(userid))
      ab <- OptionT(helloify(a))
    } yield ab
    result.opt
  }


}
