package chess.recursive

import chess.common.Domain

object Algorithm {
  def apply(domain: Domain) = {
    new Algorithm(domain).search(domain.initialBoard)
  }
}

class Algorithm(domain: Domain) {

  import Domain._
  import domain.dim

  def search(board: Board): List[List[(Int, Piece)]] =
    if (board.piecesLeft.isEmpty)
      board.piecesPut :: Nil
    else if (board.safeIndices.isEmpty)
      Nil
    else {
      val skipIndex = search(board.skipIndex)
      val tryIndex = for {
        p <- board.piecesLeft.pop
        b <- board.withPiece(p).toList
        s <- search(b)
      } yield s

      skipIndex ::: tryIndex
    }
}