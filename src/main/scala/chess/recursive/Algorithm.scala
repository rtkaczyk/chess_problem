package chess.recursive

import chess.common.Domain
import Domain._

object Algorithm {
  def apply(domain: Domain) = {
    new Algorithm(domain).solve
  }
}

class Algorithm(domain: Domain) {
  def solve = {
    val Frame(b, ps, sq) = domain.initialFrame
    search(b, ps, sq)
  }

  def search(board: Board, pieceSet: PieceSet, currSq: Option[Square])
  : List[Map[Square, Piece]] = {

    if (pieceSet.isEmpty)
      board.pieces :: Nil

    else if (currSq.isEmpty || domain.remainingSafe(board, currSq.get) < pieceSet.size)
      Nil

    else {
      val sq = currSq.get

      val skipSquare = search(board, pieceSet, domain.nextSquare(board, sq))
      val trySquare = for {
        p <- pieceSet.pop
        b <- domain.boardWithPiece(board, p, sq)
      } yield search(b, pieceSet - p, domain.nextSquare(b, sq))

      skipSquare ::: trySquare.flatten
    }
  }
}