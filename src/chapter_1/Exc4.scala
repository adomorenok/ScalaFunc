package chapter_1

// uncurry function
object Exc4 {

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

}
