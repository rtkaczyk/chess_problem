package chess.parallel

import chess.common.Domain._

object Protocol {
  case object Continue
  case object Share
  case class Finished(solutions: List[Map[Square, Piece]])
  case class Frame(board: Board, pieceSet: PieceSet, currSq: Option[Square])
}