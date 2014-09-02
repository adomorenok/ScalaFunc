package chapter_1

import chapter_1.Exc2.partial1

// currying function
object Exc3 extends App {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
//    (a: A) => partial1(a, f)

  // testing
  val sum = curry[Int,Int,Int](_ + _)
  val multiplication = curry[Int,Int,Int](_ * _)
  println(sum(1)(2))
  println(multiplication(8)(4))
}
