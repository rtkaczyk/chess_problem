package chess.common

object IO {
  def input(): Domain = {
    val files :: ranks :: k :: q :: r :: b :: n :: Nil =
      io.StdIn.readLine().split("\\s+").map(_.toInt).toList

    Domain(files, ranks, k, q, r , b, n)
  }
  
  def output(solutions: => Traversable[_]) {
    val (r, t) = timeIt(solutions)
    println(s"Solutions: ${r.size} (time: $t ms)")
  }

  def timeIt[R](r: => R): (R, Long) = {
    val t0 = System.currentTimeMillis
    (r, System.currentTimeMillis - t0)
  }
}