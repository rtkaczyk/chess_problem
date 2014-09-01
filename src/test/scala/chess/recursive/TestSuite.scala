package chess

import org.scalatest._
import chess.common.Domain._
import chess.recursive.Algorithm

class TestSuite extends FunSuite {
  
  //Not using TableDrivenPropertyChecks cause I want
  //each test problem timed individually
  def testProblem(pn: (Problem, Int)) {
    val (problem, n) = pn
    test(s"[$problem] == $n") {
      assert(Algorithm(problem).size === n)
    }
  }
  
  val problems = Seq(
    Problem(2, 2, kings = 1, bishops = 1) -> 0,
    Problem(3, 3, kings = 2, rooks = 1) -> 4,
    Problem(2, 5, rooks = 2) -> 20,
    Problem(4, 4, rooks = 2, knights = 4) -> 8,
    Problem(4, 4, bishops = 6) -> 16,
    Problem(4, 4, queens = 1, knights = 2) -> 40,
    Problem(6, 6, queens = 6) -> 4,
    Problem(7, 7, queens = 7) -> 40,
    Problem(8, 8, queens = 8) -> 92/*,
    Problem(9, 9, queens = 9) -> 352,
    Problem(8, 8, queens = 5, knights = 5) -> 16,
    Problem(7, 7, kings = 2, queens = 2, bishops = 2, knights = 1) -> 3063828*/
  )
  
  problems foreach testProblem
}