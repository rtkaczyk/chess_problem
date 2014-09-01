package chess.recursive

import chess.common.Domain._

import scala.collection.mutable

object Algorithm {

  val cache = mutable.Map[(Piece, Square), Set[Square]]()
  
  def apply(problem: Problem): List[Map[Square, Piece]] = {
    cache.clear()
    search(problem.board, problem.pieceSet, Some(0, 0))
  }

  def search(board: Board, pieceSet: PieceSet, currSq: Option[Square])
  : List[Map[Square, Piece]] = {
    
    if (pieceSet.isEmpty)
      board.pieces :: Nil
    
    else if (currSq.isEmpty || board.safe < pieceSet.size || 
        board.remaining(currSq.get) < pieceSet.size)
      Nil
    
    else {
      val sq = currSq.get
      
      val skipSquare = search(board, pieceSet, board.nextSquare(sq))
      val trySquare = for {
        p <- pieceSet.pop
        b <- board.withPiece(p, sq, moves(p, board, sq))
      } yield search(b, pieceSet - p, b.nextSquare(sq))
      
      trySquare.flatten ++ skipSquare
    }
  }

  def moves(p: Piece, b: Board, sq: Square): Set[Square] =
    cache.getOrElseUpdate((p, sq), p(b, sq))
}