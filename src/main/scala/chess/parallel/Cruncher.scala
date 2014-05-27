package chess.parallel

import akka.actor._
import chess.common.Domain._
import collection.mutable
import Protocol._

class Cruncher(coordinator: ActorRef) extends Actor {
  
  val stack = mutable.Stack[Frame]()
  var solutions = List[Map[Square, Piece]]()
  
  def receive = {
    case f: Frame =>
      stack.push(f)
      search()
  
    case Continue =>
      if (stack.nonEmpty) {
        search()
      } else {
        coordinator ! Finished(solutions)
        solutions = Nil
      }
      
    case Share =>
      if (stack.nonEmpty)
        coordinator ! stack.pop()
  }
  
  def search() {
    
    val Frame(board, pieceSet, currSq) = stack.pop()
    
    if (pieceSet.isEmpty)
      solutions ::= board.pieces
    
    else if (currSq.isEmpty || board.safe < pieceSet.size || 
        board.remaining(currSq.get) < pieceSet.size)
      ()
    
    else {
      val sq = currSq.get
      
      stack.push(Frame(board, pieceSet, board.nextSquare(sq)))
      for (p <- pieceSet.pop; b <- board.withPiece(p, sq))
        stack.push(Frame(b, pieceSet - p, b.nextSquare(sq)))
    }
      
    self ! Continue 
  } 
}