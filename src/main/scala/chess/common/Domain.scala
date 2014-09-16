package chess.common

import language.implicitConversions

object Domain {
  case class Square(file: Int, rank: Int)

  case class Dim(files: Int, ranks: Int) {
    val size = files * ranks
  }

  sealed trait Piece {
    def attacks(from: Square, target: Square): Boolean
  }

  case object Rook extends Piece {
    def attacks(from: Square, target: Square): Boolean =
      from.file == target.file || from.rank == target.rank
  }

  case object Bishop extends Piece {
    def attacks(from: Square, target: Square): Boolean =
      (from.file - target.file).abs == (from.rank - target.rank).abs
  }

  case object Queen extends Piece {
    def attacks(from: Square, target: Square): Boolean =
      Rook.attacks(from, target) || Bishop.attacks(from, target)
  }

  case object King extends Piece {
    def attacks(from: Square, target: Square): Boolean =
      (from.file - target.file).abs <= 1 && (from.rank - target.rank).abs <= 1
  }

  case object Knight extends Piece {
    def attacks(from: Square, target: Square): Boolean = {
      val df = (from.file - target.file).abs
      val dr = (from.rank - target.rank).abs
      df + dr == 3 && df > 0 && dr > 0
    }
  }

  val pieceOrder = King :: Queen :: Rook :: Bishop :: Knight :: Nil

  class PieceSet(pieces: List[Int]) {
    def pop: List[Piece] = {
      val pcs = for ((n, p) <- pieces zip pieceOrder)
      yield if (n > 0) Some(p) else None

      pcs.flatten
    }

    val size = pieces.sum
    val isEmpty = size == 0

    def - (p: Piece): PieceSet = {
      val i = pieceOrder.indexOf(p)
      val n = pieces(i)
      new PieceSet(pieces.updated(i, n - 1))
    }
  }

  case class Board(piecesPut: List[(Int, Piece)], piecesLeft: PieceSet, safeIndices: List[Int]) {
    def withPiece(piece: Piece)(implicit dim: Dim): Option[Board] = safeIndices match {
      case Nil =>
        None

      case idx :: idxs =>
        val sq = idx2sq(idx)
        val captures = piecesPut.exists(pp => piece.attacks(sq, idx2sq(pp._1)))
        if (captures)
          None
        else {
          val remainingIdxs = idxs.filter(i => !piece.attacks(sq, idx2sq(i)))
          Some(Board((idx, piece) :: piecesPut, piecesLeft - piece, remainingIdxs))
        }
    }

    def skipIndex: Board =
      this.copy(safeIndices = safeIndices.tail)

    def idx2sq(idx: Int)(implicit dim: Dim): Square =
      Square(idx / dim.ranks, idx % dim.ranks)
  }
}

case class Domain(files: Int, ranks: Int, kings: Int = 0, queens: Int = 0,
                  rooks: Int = 0, bishops: Int = 0, knights: Int = 0) {

  import Domain._

  implicit val dim = Dim(files, ranks)

  val initialBoard = Board(
    Nil,
    new PieceSet(kings :: queens :: rooks :: bishops :: knights :: Nil),
    0.until(files * ranks).toList)
}