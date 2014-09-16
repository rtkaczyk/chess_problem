package chess.parallel

import akka.actor._
import chess.common.IO
import chess.parallel.Solver.Continue

object Chess extends App {
  val domain = IO.input()
  val system = ActorSystem("chess")
  val solver = system.actorOf(Props(classOf[Solver], domain), "solver")

  solver ! Continue(Nil, domain.initialBoard :: Nil)
}