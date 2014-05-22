package chess

import Domain._

object Algorithm {
  
  def apply(m: Int, n: Int, pcs: List[Int]): List[Map[Square, Piece]] =
    search(Board(m, n), PieceSet(pcs), Some(0, 0))

  def search(board: Board, pieceSet: PieceSet, currSq: Option[Square])
  : List[Map[Square, Piece]] = {
    
    if (pieceSet.isEmpty)
      board.pieces :: Nil
    
    else if (currSq.isEmpty)
      Nil
    
    else {
      val sq = currSq.get
      
      val skipSquare = search(board, pieceSet, board.nextSquare(sq))
      val trySquare = for (p <- pieceSet.pop; b <- board.withPiece(p, sq))
        yield search(b, pieceSet - p, b.nextSquare(sq))
      
      trySquare.flatten ++ skipSquare
    }
  } 
}