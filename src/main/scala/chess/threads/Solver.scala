package chess.threads

import chess.common.Domain._
import Chess.{stack, idling}
import chess.parallel.Protocol.Frame

class Solver extends Thread {
  
  private var _running = false
  def running = _running
  
  private var stopRequested = false
  def kill() = stopRequested = true
  
  private var _solutions = List[Map[Square, Piece]]() 
  def solutions = _solutions
  
  override def run {
    while(!stopRequested) {
      search()
    }
  }
  
  def search() {
    val frame = stack.pollFirst()
    
    if (frame == null) {
      _running = false
      idling()
      Thread.`yield`()
    }
    else {
      _running = true
      val Frame(board, pieceSet, currSq) = frame
      
      if (pieceSet.isEmpty)
        _solutions ::= board.pieces
      
      else if (currSq.isEmpty || board.safe < pieceSet.size || 
          board.remaining(currSq.get) < pieceSet.size)
        ()
      
      else {
        val sq = currSq.get
        
        stack.addFirst(Frame(board, pieceSet, board.nextSquare(sq)))
        for (p <- pieceSet.pop; b <- board.withPiece(p, sq))
          stack.addFirst(Frame(b, pieceSet - p, b.nextSquare(sq)))
      }
    }
  }
}