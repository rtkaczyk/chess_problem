package chess.common

import language.implicitConversions

object Domain {
  case class Square(file: Int, rank: Int)

  case class Dim(files: Int, ranks: Int) {
    val size = files * ranks
    val initialIndices = 0.until(size).toList
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

  type PiecesOnBoard = List[(Int, Piece)]

  case class Board(piecesPut: PiecesOnBoard, piecesLeft: List[Piece], safeIndices: List[Int])(implicit dim: Dim) {

    def withPiece: List[Board] = {
      val (piece :: remPieces) = piecesLeft

      def putPiece(acc: List[Board], remIdxs: List[Int]): List[Board] = remIdxs match {
        case Nil =>
          acc

        case idx :: idxs =>
          val sq = idx2sq(idx)
          if (piecesPut.exists(pp => piece.attacks(sq, idx2sq(pp._1))))
            putPiece(acc, idxs)
          else {
            val nextIdxs =
              (if (indicesFromTop.nonEmpty) indicesFromTop else idxs)
                .filterNot(i => i == idx || piece.attacks(sq, idx2sq(i)))

            val newBoard = Board((idx, piece) :: piecesPut, remPieces, nextIdxs)
            putPiece(newBoard :: acc, idxs)
          }
      }

      putPiece(Nil, safeIndices)
    }

    def withPieceI: List[Board] = {
      var boards = List[Board]()
      val piece :: remPieces = piecesLeft
      var remIdxs = safeIndices

      while(remIdxs.nonEmpty) {
        val sq = idx2sq(remIdxs.head)
        if(!piecesPut.exists(pp => piece.attacks(sq, idx2sq(pp._1)))) {
          val nextIdxs =
            (if (indicesFromTop.nonEmpty) indicesFromTop else remIdxs.tail)
              .filterNot(i => i == remIdxs.head || piece.attacks(sq, idx2sq(i)))

          val newBoard = Board((remIdxs.head, piece) :: piecesPut, remPieces, nextIdxs)
          boards = newBoard :: boards
        }
        remIdxs = remIdxs.tail
      }

      boards
    }

    private lazy val indicesFromTop: List[Int] = piecesLeft match {
      case p1 :: p2 :: _ if p1 != p2 =>
        dim.initialIndices.filterNot(idx => piecesPut.exists {
          case (i, p) => idx == i || p.attacks(idx2sq(i), idx2sq(idx))
        })
      case _ => Nil
    }

    private def idx2sq(idx: Int): Square =
      Square(idx / dim.ranks, idx % dim.ranks)

    def repr: String = {
      val pieceRepr = Map[Piece, String](King -> "K", Queen -> "Q", Rook -> "R", Bishop -> "B", Knight -> "N")
      val pieces = piecesPut.toMap
      val safe = safeIndices.toSet

      val boardRepr =
        (for (r <- 0 until dim.ranks) yield {
          (for (f <- 0 until dim.files) yield {
            val idx = f * dim.ranks + r
            pieces.get(idx).map(pieceRepr).getOrElse(if (safe contains idx) "." else "-")
          }).mkString(" ")
        }).mkString("\n")

      boardRepr + "\n"
    }
  }
}

case class Domain(files: Int, ranks: Int, kings: Int = 0, queens: Int = 0,
                  rooks: Int = 0, bishops: Int = 0, knights: Int = 0) {

  import Domain._

  implicit val dim = Dim(files, ranks)

  val pieces = List (
    Queen -> queens,
    Rook -> rooks,
    Bishop -> bishops,
    King -> kings,
    Knight -> knights
  ).flatMap { case (p, n) => List.fill(n)(p)}

  val initialBoard = Board(Nil, pieces, dim.initialIndices)
}