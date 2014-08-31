package chess.parallel

import akka.actor._
import chess.common.IO
import Protocol._
import com.typesafe.config.ConfigFactory

object Chess extends App {
  val system = ActorSystem("chess")
  val coordinator = system.actorOf(Props(classOf[Coordinator]), "coordinator")
  
  val problem = IO.input()
  
  coordinator ! Frame(problem.board, problem.pieceSet, Some(0, 0))
}