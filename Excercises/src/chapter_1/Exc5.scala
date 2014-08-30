package chapter_1

// higher order function, which composes two functions
object Exc5 extends App{

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  val t = compose[Int,Int,Int](_ * 3, _ + 1)
  println(t(3))
}


