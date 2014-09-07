package chess.common

import language.implicitConversions

object Domain {
  case class Square(f: Int, r: Int) {
    def + (relative: Square) = Square(f + relative.f, r + relative.r)

    def - (relative: Square) = Square(f - relative.f, r - relative.r)
  }

  implicit def tuple2Square(fr: (Int, Int)) = Square(fr._1, fr._2)

  sealed trait Piece

  case object Rook extends Piece

  case object Bishop extends Piece {
    val dirs = Set[Square]((1, 1), (1, -1), (-1, -1), (-1, 1))
  }

  case object Queen extends Piece

  case object King extends Piece {
    val moves = Set[Square]((1, 0), (1, 1), (0, 1), (-1, 1))
  }

  case object Knight extends Piece {
    val moves = Set[Square]((2, 1), (1, 2), (-1, 2), (-2, 1))
  }

  val pieceChoice = King :: Queen :: Rook :: Bishop :: Knight :: Nil

  case class PieceSet(pieces: List[Int]) {
    def pop: List[Piece] = {
      val pcs = for ((n, p) <- pieces zip pieceChoice)
      yield if (n > 0) Some(p) else None

      pcs.flatten
    }

    val size = pieces.sum
    val isEmpty = size == 0

    def - (p: Piece): PieceSet = {
      val i = pieceChoice.indexOf(p)
      val n = pieces(i)
      PieceSet(pieces.updated(i, n - 1))
    }
  }

  case class Board(pieces: Map[Square, Piece], threatened: Set[Square]) {
    def safe(sq: Square): Boolean =
      !threatened(sq)
  }

  case class Frame(board: Board, pieceSet: PieceSet, currSq: Option[Square])
}

case class Domain(files: Int, ranks: Int, kings: Int = 0, queens: Int = 0,
                  rooks: Int = 0, bishops: Int = 0, knights: Int = 0) {

  import Domain._

  val initialFrame = Frame(Board(Map(), Set()),
    PieceSet(kings :: queens :: rooks :: bishops :: knights :: Nil),
    Some(Square(0, 0)))

  val memo: Map[(Piece, Square), Set[Square]] = {
    for {
      sq <- sqIterator(0, 0)
      p <- initialFrame.pieceSet.pop
    } yield (p, sq) -> pieceMoves(p, sq)
  }.toMap
    
  def inside(sq: Square): Boolean =
    sq.f >= 0 && sq.r >= 0 && sq.f < files && sq.r < ranks

  def sqIterator(square: Square): Iterator[Square] =
    Iterator.iterate(square) { sq =>
      if (sq.r + 1 < ranks)
        (sq.f, sq.r + 1)
      else
        (sq.f + 1, 0)
    } takeWhile inside

  def nextSquare(board: Board, square: Square): Option[Square] =
    sqIterator(square).drop(1).find(board.safe)

  def boardWithPiece(board: Board, p: Piece, sq: Square): Option[Board] = {
    val moves = memo((p, sq))
    val captures = moves.exists(board.pieces.isDefinedAt)

    if (captures)
      None
    else
      Some(Board(board.pieces + (sq -> p), board.threatened ++ moves))
  }

  def remainingSafe(board: Board, sq: Square): Int =
    sqIterator(sq).count(board.safe)

  def pieceMoves(p: Piece, sq: Square): Set[Square] = p match {
    case Rook =>
      val horz = for (f <- 0 until files) yield Square(f, sq.r)
      val vert = for (r <- 0 until ranks) yield Square(sq.f, r)
      (horz ++ vert).toSet - sq

    case Bishop =>
      for {
        d <- Bishop.dirs
        m <- Iterator.iterate(sq)(_ + d) drop 1 takeWhile inside
      } yield m

    case Queen =>
      pieceMoves(Bishop, sq) ++ pieceMoves(Rook, sq)

    case King =>
      (King.moves.map(sq.+) ++ King.moves.map(sq.-)) filter inside

    case Knight =>
      (Knight.moves.map(sq.+) ++ Knight.moves.map(sq.-)) filter inside
  }
}