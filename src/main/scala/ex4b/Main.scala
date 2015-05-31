package ex4b

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** Let's group those functions into traits */
object Main extends App {

  import scala.language.higherKinds

  trait ToFlatmappable[M[_]] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  trait ToInstantiatable[M[A]] {
    def apply[A](a: A): M[A]
  }

  case class OptionT[F[_], A](opt: F[Option[A]]) {

    def flatMap[B, C, D](f: A => OptionT[F, B])
                        (implicit FM: ToFlatmappable[F],
                                  FI: ToInstantiatable[F]): OptionT[F, B] = OptionT[F, B] {
      FM.flatMap(opt) {
          case Some(a) => f(a).opt
          case None => FI.apply(None: Option[B])
        }
    }

    def map[B](f: A => B)
              (implicit FM: ToFlatmappable[F],
                        FI: ToInstantiatable[F]): OptionT[F, B] = {
      flatMap { a => OptionT[F, B](FI.apply(Some(f(a)))) }
    }
  }


  def helloifyUser4(userid: Int): Future[Option[String]] = {
    implicit val futureInstantiable:ToInstantiatable[Future] = new ToInstantiatable[Future] {
      override def apply[A](a: A): Future[A] = Future.successful(a)
    }

    implicit val futureFlatmappable = new ToFlatmappable[Future] {
      override def flatMap[A, B](ma: Future[A])(f: (A) => Future[B]): Future[B] = ma.flatMap(f)
    }

    val result = for {
      a <- OptionT(findUser(userid))
      ab <- OptionT(helloify(a))
    } yield ab
    result.opt
  }


}
