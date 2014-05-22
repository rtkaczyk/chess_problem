package chess

import Domain._

object Test extends App {
  println(Algorithm(3, 3, 2 :: 0 :: 0 :: 1 :: Nil).size)
  println(Algorithm(4, 4, 0 :: 0 :: 0 :: 2 :: 4 :: Nil).size)
  println(Algorithm(8, 8, 0 :: 8 :: Nil).size)
}