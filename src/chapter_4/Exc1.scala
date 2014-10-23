package chapter_4

import chapter_4.auxiliary.{MyStream => ms}

object Exc1 extends App {
  def pair(i: => Int) = (i, i)

  ms.cons(1, ms(1, ms.empty))
  pair {println("hello"); 41 + 1}

  val x = ms(1, 3, 4, 6)
  println(x.toList)
  println(x.take(2).toList)
  println(x.takeWhile(_ < 4 ).toList)
}
