package chess.remote

import akka.actor.ActorSystem
import chess.parallel.Deployment

object Chess extends App {
  ActorSystem(Deployment.SysName)
}