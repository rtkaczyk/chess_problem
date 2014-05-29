package chess.common

import language.implicitConversions

object Domain {
  
  case class Problem(m: Int, n: Int, kings: Int = 0, queens: Int = 0,
      bishops: Int = 0, rooks: Int = 0, knights: Int = 0) {
    
    def board = Board(m, n)
    
    def pieceSet = PieceSet(kings :: queens :: bishops :: rooks :: knights :: Nil)
    
    override def toString = {
      val pieces = (pieceSet.pieces zip pieceSet.choice).filter(_._1 > 0)
        .map{case (n, p) => s"$n${p.notation}"}.mkString(", ")
      s"${m}x$n: $pieces"
    }
  }
  
  case class Square(f: Int, r: Int) {
    def + (relative: Square) = Square(f + relative.f, r + relative.r)
    
    def - (relative: Square) = Square(f - relative.f, r - relative.r)
    
    def notation: String = ('a' to 'z')(f).toString + (r + 1) 
    
    def tuple = (f, r)
  }
  
  implicit def tuple2Square(fr: (Int, Int)) = Square(fr._1, fr._2)
  
  case class Board(files: Int, ranks: Int, 
      pieces: Map[Square, Piece] = Map(), threatened: Set[Square] = Set()) {
    
    def inside(sq: Square): Boolean =
      sq.f >= 0 && sq.r >= 0 && sq.f < files && sq.r < ranks
    
    def sqIterator(square: Square): Iterator[Square] =
      Iterator.iterate(square) { sq => 
        if (sq.r + 1 < ranks)
          (sq.f, sq.r + 1)
        else
          (sq.f + 1, 0)
      } drop 1 takeWhile inside
    
    def nextSquare(square: Square): Option[Square] = 
      sqIterator(square).find(sq => !threatened(sq))
      
    def withPiece(p: Piece, sq: Square): Option[Board] = {
      val moves = p(this)(sq)
      val captures = moves.exists(pieces.isDefinedAt)
      
      if (captures)
        None
      else
        Some(Board(files, ranks, pieces + (sq -> p), threatened ++ moves))
    }
    
    def safe: Int = files * ranks - (threatened.size + pieces.size)
    
    def remaining(sq: Square) = (files - sq.f) * ranks - sq.r 
  }
      
  case class PieceSet(pieces: List[Int]) {
    
    val choice = King :: Queen :: Bishop :: Rook :: Knight :: Nil
    
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
    def apply(b: Board)(sq: Square): Set[Square]
    def notation: String
  }
  
  case object Rook extends Piece {
    def apply(b: Board)(sq: Square) = {
      val horz = for (f <- 0 until b.files) yield Square(f, sq.r)
      val vert = for (r <- 0 until b.ranks) yield Square(sq.f, r)
      
      (horz ++ vert).toSet - sq
    }
    
    def notation = "R"
  }
  
  case object Bishop extends Piece {
    val dirs = Set[Square]((1, 1), (1, -1), (-1, -1), (-1, 1))

    def apply(b: Board)(sq: Square) =
      for {
        d <- dirs 
        m <- Iterator.iterate(sq)(_ + d) drop 1 takeWhile b.inside 
      } yield m
      
    def notation = "B"
  }
  
  case object Queen extends Piece {
    def apply(b: Board)(sq: Square) = 
      Rook(b)(sq) ++ Bishop(b)(sq)
      
    def notation = "Q"
  }
  
  case object King extends Piece {
    val moves = Set[Square]((1, 0), (1, 1), (0, 1), (-1, 1))
    
    def apply(b: Board)(sq: Square) = 
      (moves.map(sq.+) ++ moves.map(sq.-)) filter b.inside
      
    def notation = "K"
  }
  
  case object Knight extends Piece {
    val moves = Set[Square]((2, 1), (1, 2), (-1, 2), (-2, 1))
    
    def apply(b: Board)(sq: Square) = 
      (moves.map(sq.+) ++ moves.map(sq.-)) filter b.inside
    
    def notation = "N"
  }
}