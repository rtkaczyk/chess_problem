package chess.recursive

import org.scalatest._
import chess.common.Domain

class TestSuite extends FunSuite {
  
  //Not using TableDrivenPropertyChecks cause I want
  //each test Domain timed individually
  def testDomain(pn: (Domain, Int)) {
    val (domain, n) = pn
    test(s"[$domain] == $n") {
      assert(Algorithm(domain).size === n)
    }
  }
  
  val domains = Seq(
    Domain(2, 2, kings = 1, bishops = 1) -> 0,
    Domain(3, 3, kings = 2, rooks = 1) -> 4,
    Domain(2, 5, rooks = 2) -> 20,
    Domain(4, 4, rooks = 2, knights = 4) -> 8,
    Domain(4, 4, bishops = 6) -> 16,
    Domain(4, 4, queens = 1, knights = 2) -> 40,
    Domain(6, 6, queens = 6) -> 4,
    Domain(7, 7, queens = 7) -> 40,
    Domain(8, 8, queens = 8) -> 92,
    Domain(9, 9, queens = 9) -> 352/*,
    Domain(8, 8, queens = 5, knights = 5) -> 16,
    Domain(7, 7, kings = 2, queens = 2, bishops = 2, knights = 1) -> 3063828*/
  )
  
  domains foreach testDomain
}