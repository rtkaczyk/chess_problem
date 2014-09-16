package chess.recursive

import chess.common.Domain

object Algorithm {
  def apply(domain: Domain) = {
    new Algorithm(domain).search(domain.initialBoard)
  }
}

class Algorithm(domain: Domain) {

  import Domain._

  def search(board: Board): List[PiecesOnBoard] =
    if (board.piecesLeft.isEmpty)
      board.piecesPut :: Nil
    else if (board.safeIndices.isEmpty)
      Nil
    else
      board.withPieces.flatMap(search)
}