package chess

import Domain._

object IO {
  def input(): Problem = {
    val files :: ranks :: k :: q :: b :: r :: n :: Nil = 
      readLine().split("\\s+").map(_.toInt).toList
    
    Problem(files, ranks, k, q, b, r, n)
  }
  
  def output(solutions: List[Map[Square, Piece]]) {
    solutions foreach { s =>
      println(s.toSeq.map{ case (sq, p) => p.notation + sq.notation }
        .mkString(" "))
    }
    
    println(solutions.size)
  }
}