package ex4a

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** Let's extract anything Future specific from our Future[Option[_]] stack into functions */
object Main extends App {


  import scala.language.higherKinds

  type ToFlatmappable[M[_],A,B] = M[A] => ((A => M[B]) => M[B])

  type InstantiateFromType[M[_],A] = A => M[A]

  case class OptionT[F[_], A](opt: F[Option[A]]) {

    def flatMap[B,C,D](f: A => OptionT[F, B])
                  (implicit flatmappify: ToFlatmappable[F,Option[A],Option[B]],
                            instantiate: InstantiateFromType[F,Option[B]]): OptionT[F, B] = OptionT[F, B]{

      flatmappify(opt){
          case Some(a) => f(a).opt
          case None => instantiate(None)
        }
    }


    def map[B](f: A => B)
              (implicit flatmappify: ToFlatmappable[F,Option[A],Option[B]],
                        instantiate: InstantiateFromType[F,Option[B]]): OptionT[F, B] = flatMap { a => OptionT[F, B](instantiate(Some(f(a)))) }
  }


  def helloifyUser4(userid: Int): Future[Option[String]] = {
    implicit def instantiate[A](a:A)=Future.successful(a)

    implicit def flatmappify[A,B](fa:Future[A])(a : A=> Future[B]):Future[B] = fa.flatMap(a)

    val result:OptionT[Future,String] = for {
      a <- OptionT(findUser(userid))
      ab <- OptionT(helloify(a))
    } yield ab
    result.opt
  }


}
