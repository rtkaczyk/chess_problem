package chess.parallel

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import chess.common.IO
import chess.parallel.Solver.Continue
import scala.concurrent.Await
import scala.concurrent.duration._

object Chess extends App {
  val domain = IO.input()
  val system = ActorSystem("chess")
  val solver = system.actorOf(Props(classOf[Solver], domain), "solver")
  implicit val timeout = Timeout(10.days)

  val solutions = solver ? Continue(Nil, domain.initialBoard :: Nil)
  IO.output(Await.result(solutions.mapTo[List[_]], timeout.duration))
}