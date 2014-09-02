package chess.common

import language.implicitConversions

case class Domain(files: Int, ranks: Int, kings: Int = 0, queens: Int = 0,
                  rooks: Int = 0, bishops: Int = 0, knights: Int = 0) {

  def initialBoard = Board()
    
  def initialPieceSet = PieceSet(kings :: queens :: rooks :: bishops :: knights :: Nil)

  def inside(sq: Square): Boolean =
    sq.f >= 0 && sq.r >= 0 && sq.f < files && sq.r < ranks

  case class Square(f: Int, r: Int) {
    def + (relative: Square) = Square(f + relative.f, r + relative.r)

    def - (relative: Square) = Square(f - relative.f, r - relative.r)
  }

  implicit def tuple2Square(fr: (Int, Int)) = Square(fr._1, fr._2)

  trait Memoizer {
    def apply(p: Piece, sq: Square): Set[Square]
  }

  case class Board(pieces: Map[Square, Piece] = Map(), threatened: Set[Square] = Set()) {
    def safe(sq: Square): Boolean =
      !threatened(sq)

    def sqIterator(square: Square): Iterator[Square] =
      Iterator.iterate(square) { sq =>
        if (sq.r + 1 < ranks)
          (sq.f, sq.r + 1)
        else
          (sq.f + 1, 0)
      } takeWhile inside

    def nextSquare(square: Square): Option[Square] =
      sqIterator(square).drop(1).find(safe)

    def withPiece(p: Piece, sq: Square)(implicit memo: Memoizer): Option[Board] = {
      val moves = memo(p, sq)
      val captures = moves.exists(pieces.isDefinedAt)

      if (captures)
        None
      else
        Some(Board(pieces + (sq -> p), threatened ++ moves))
    }

    def remainingSafe(sq: Square): Int =
      sqIterator(sq).count(safe)
  }
      
  case class PieceSet(pieces: List[Int]) {
    
    val choice = King :: Queen :: Rook :: Bishop :: Knight :: Nil
    
    def pop: List[Piece] = {
      val pcs = for ((n, p) <- pieces zip choice) 
        yield if (n > 0) Some(p) else None
      
      pcs.flatten
    }
    
    val size = pieces.sum
    
    val isEmpty = size == 0
    
    def - (p: Piece): PieceSet = {
      val i = choice.indexOf(p)
      val n = pieces(i)
      PieceSet(pieces.updated(i, n - 1))
    } 
  }
  
  sealed trait Piece {
    def apply(sq: Square): Set[Square]
  }
  
  case object Rook extends Piece {
    def apply(sq: Square) = {
      val horz = for (f <- 0 until files) yield Square(f, sq.r)
      val vert = for (r <- 0 until ranks) yield Square(sq.f, r)
      
      (horz ++ vert).toSet - sq
    }
  }
  
  case object Bishop extends Piece {
    val dirs = Set[Square]((1, 1), (1, -1), (-1, -1), (-1, 1))

    def apply(sq: Square) =
      for {
        d <- dirs 
        m <- Iterator.iterate(sq)(_ + d) drop 1 takeWhile inside
      } yield m
  }
  
  case object Queen extends Piece {
    def apply(sq: Square) =
      Rook(sq) ++ Bishop(sq)
  }
  
  case object King extends Piece {
    val moves = Set[Square]((1, 0), (1, 1), (0, 1), (-1, 1))
    
    def apply(sq: Square) =
      (moves.map(sq.+) ++ moves.map(sq.-)) filter inside
  }
  
  case object Knight extends Piece {
    val moves = Set[Square]((2, 1), (1, 2), (-1, 2), (-2, 1))
    
    def apply(sq: Square) =
      (moves.map(sq.+) ++ moves.map(sq.-)) filter inside
  }
}