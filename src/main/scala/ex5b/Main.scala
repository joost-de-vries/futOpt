package ex5b

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** Using a context bound removes the implicit from the function signature and thus
  * hides the type class for client developers.  */
object Main extends App {

  import scala.language.higherKinds

  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def apply[A](a: A): F[A]
  }

  case class OptionT[F[_] : Monad, A](opt: F[Option[A]]) {

    def flatMap[B](f: A => OptionT[F, B]): OptionT[F, B] = {
      val FF = implicitly[Monad[F]]
      OptionT[F, B](
        FF.flatMap(opt) {
          case Some(a) => f(a).opt
          case None => FF.apply(None: Option[B])
        }
      )
    }

    def map[B](f: A => B): OptionT[F, B] = {
      val FF = implicitly[Monad[F]]
      flatMap { a => OptionT[F, B](FF.apply(Some(f(a)))) }
    }
  }


  object FutureMonad {

    implicit val typeClass = new Monad[Future] {
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f(_))

      def apply[A](a: A): Future[A] = Future.successful(a)
    }
  }

  def helloifyUser4(userid: Int): Future[Option[String]] = {
    import FutureMonad.typeClass

    val result = for {
      a <- OptionT(findUser(userid))
      ab <- OptionT(helloify(a))
    } yield ab
    result.opt
  }
}
