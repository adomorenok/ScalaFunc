package chapter_1

// partial application
object Exc2 extends App {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)


  // testing
  val part1 = partial1[Int, Int, Int](1, _ + _)
  println(part1(3))
  val part2 = partial1[String, Int, String]("Hello+", _ + _)
  println(part2(1))

}
