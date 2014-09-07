package chess.parallel


import akka.actor.{PoisonPill, Actor}
import chess.common._
import chess.common.Domain._

import scala.concurrent.Future

object Solver {
  case class Continue(solutions: List[Map[Square, Piece]], frames: List[Frame])
  case class Solutions(s: List[Map[Square, Piece]])
}

class Solver(val domain: Domain) extends Actor {

  import context.dispatcher
  import Solver._


  val MaxActiveFutures = System.getProperty("max.active.futures", "4").toInt
  val FramesPerFuture = System.getProperty("frames.per.future", "64").toInt

  var solutions = List[Map[Square, Piece]]()
  var stack = List[Frame]()
  var active = 1 //first frame will not come from a future, but I'm treating it as such


  def receive = {
    case Continue(ss, fs) =>
      solutions :::= ss
      stack :::= fs
      active -= 1

      if (active == 0 && stack.isEmpty) {
        IO.output(solutions)
        context.system.shutdown()
      } else
        fireFutures()
  }

  def fireFutures() = while (active < MaxActiveFutures && stack.nonEmpty) {
    val (frames, tail) = stack.splitAt(FramesPerFuture)
    stack = tail
    active += 1
    Future {
      val (solved, unsolved) = frames.partition(_.pieceSet.isEmpty)
      self ! Continue(solved.map(_.board.pieces), unsolved.flatMap(search))
    }
  }

  def search(frame: Frame): List[Frame] = {
    val Frame(board, pieceSet, currSq) = frame

    currSq.map { sq =>
      if (domain.remainingSafe(board, sq) < pieceSet.size)
        Nil
      else {
        val skipSquare = Frame(board, pieceSet, domain.nextSquare(board, sq))
        val trySquare = for (p <- pieceSet.pop; b <- domain.boardWithPiece(board, p, sq))
          yield Frame(b, pieceSet - p, domain.nextSquare(b, sq))

        skipSquare :: trySquare
      }
    } getOrElse Nil
  }

}
