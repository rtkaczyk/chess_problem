package chess.common

object IO {
  def input(): Domain = {
    val files :: ranks :: k :: q :: r :: b :: n :: Nil =
      io.StdIn.readLine().split("\\s+").map(_.toInt).toList

    Domain(files, ranks, k, q, r , b, n)
  }
  
  def output(solutions: Traversable[_]) {
    println(solutions.size)
  }
}