package chess.parallel


import akka.actor.Actor
import chess.common._
import chess.common.Domain._

import scala.concurrent.Future

object Solver {
  case class Continue(solutions: List[List[(Int, Piece)]], frames: List[Board])
}

class Solver(val domain: Domain) extends Actor {

  import context.dispatcher
  import domain.dim
  import Solver._

  val MaxActiveFutures = System.getProperty("max.active.futures", "4").toInt
  val BoardsPerFuture = System.getProperty("boards.per.future", "64").toInt

  var solutions = List[List[(Int, Piece)]]()
  var stack = List[Board]()
  var active = 1


  def receive = {
    case Continue(ss, bs) =>
      solutions :::= ss
      stack :::= bs
      active -= 1

      if (active == 0 && stack.isEmpty) {
        IO.output(solutions)
        context.system.shutdown()
      } else
        fireFutures()
  }

  def fireFutures() = while (active < MaxActiveFutures && stack.nonEmpty) {
    val (boards, tail) = stack.splitAt(BoardsPerFuture)
    stack = tail
    active += 1
    Future {
      val (solved, unsolved) = boards.partition(_.piecesLeft.isEmpty)
      self ! Continue(solved.map(_.piecesPut), unsolved.flatMap(search))
    }
  }

  def search(board: Board): List[Board] =
    if (board.safeIndices.isEmpty)
      Nil
    else {
      val trySquare =
        for (p <- board.piecesLeft.pop; b <- board.withPiece(p))
          yield b

      board.skipIndex :: trySquare
    }
}
