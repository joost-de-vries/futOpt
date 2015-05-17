package ex3

import shared.Helloify._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Main extends App {

  //it turns out that we don't have know that it is a Future, only that it is something
  // that can be flatmapped
  // so we can generalize our FutureOpt to a 'MonadOpt'
  //i.e. a so called monad transformer. f.i. scalaz.OptionT
  def helloifyUser3(userid: Int): Future[Option[String]] = {
    import scalaz.OptionT._
    import scalaz.Scalaz._
    val result = for {
      a <- optionT(findUser(userid))
      ab <- optionT(helloify(a))
    } yield ab
    result.run
  }


}
