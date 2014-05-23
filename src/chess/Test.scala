package chess

import Domain._

object Test extends App {
  
  implicit class TestProblem(p: Problem) {
    def ~ (n: Int) = {
      val res = Algorithm(p).size
      println(if (res == n) "OK!" else s"$res did not equal $n")
    }
  }
  
  Problem(3, 3, kings = 2, rooks = 1) ~ 4
  Problem(4, 4, rooks = 2, knights = 4) ~ 8
  Problem(6, 6, queens = 6) ~ 4
  Problem(7, 7, queens = 7) ~ 40
  Problem(8, 8, queens = 8) ~ 92
  Problem(9, 9, queens = 9) ~ 352
  //takes too long
  //Problem(8, 8, queens = 5, knights = 5) ~ 16
  //Problem(7, 7, kings = 2, queens = 2, bishops = 2, knights = 1) ~ 1
  
}