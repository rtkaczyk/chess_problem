package chess.parallel

import akka.actor._
import chess.common.Domain._
import collection.mutable
import Protocol._

class Cruncher(coordinator: ActorRef) extends Actor with ActorLogging {
  
  val stack = mutable.Stack[Frame]()
  var solutions = List[Map[Square, Piece]]()
  
  val optimisationFactor = System.getProperty("optimise", "1").toInt
  var optimisationCounter = optimisationFactor 
  
  //val coordinator: ActorRef = context.parent
  
  def receive = idle
  
  def idle: Receive = {
    case f: Frame =>
      //log.info("Received a frame from: %s. Crunching".format(sender().path.name))
      context become active
      stack.push(f)
      search()
  }
  
  def active: Receive = {
    case Continue =>
      if (stack.nonEmpty) {
        //log.info("Continuing")
        search()
      } else {
        //log.info("Finishing")
        context become idle
        coordinator ! Finished(solutions.toList)
        solutions = Nil
      }
      
    case Share =>
      //log.info("Received a Share request")
      if (stack.nonEmpty)
        coordinator ! stack.pop()
  }
  
  def search() {
    
    val Frame(board, pieceSet, currSq) = stack.pop()
    
    if (pieceSet.isEmpty)
      solutions = board.pieces :: solutions
    
    else if (currSq.isEmpty || board.safe < pieceSet.size || 
        board.remaining(currSq.get) < pieceSet.size)
      ()
    
    else {
      val sq = currSq.get
      
      stack.push(Frame(board, pieceSet, board.nextSquare(sq)))
      for (p <- pieceSet.pop; b <- board.withPiece(p, sq))
        stack.push(Frame(b, pieceSet - p, b.nextSquare(sq)))
    }
    
    if (optimisationCounter > 0 && stack.nonEmpty) {
      optimisationCounter -= 1
      search()
    } else {
      optimisationCounter = optimisationFactor
      self ! Continue 
    }
  } 
}