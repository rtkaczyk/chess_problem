package chess.common

import Domain._

object IO {
  def input(): Problem = {
    val files :: ranks :: k :: q :: r :: b :: n :: Nil =
      io.StdIn.readLine().split("\\s+").map(_.toInt).toList
    
    Problem(files, ranks, k, q, r , b, n)
  }
  
  def output(solutions: Traversable[_]) {
    println(solutions.size)
  }
}