package chess.parallel

import akka.actor._

class Coordinator extends Actor {
  def receive = {
    case x => println(x)
  }
}