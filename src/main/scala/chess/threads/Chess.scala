package chess.threads

import java.util.concurrent.LinkedBlockingDeque
import chess.parallel.Protocol.Frame
import chess.common.IO
import chess.common.Domain._

object Chess extends App {
  
  val stack = new LinkedBlockingDeque[Frame]()
  val solvers = (1 to System.getProperty("app.solvers", "8").toInt)
    .map(_ => new Solver)  
  
  val problem = IO.input()
  stack.add(Frame(problem.board, problem.pieceSet, Some(0, 0)))
  solvers.foreach(_.start)
  
  lazy val output: Unit = IO.output((List[Map[Square, Piece]]() /: solvers) {
    (solutions, solver) => solver.solutions ::: solutions
  })
  
  def idling() {
    if(solvers.forall(!_.running)) {
      solvers.foreach(_.kill)
      output
    }
  }
}