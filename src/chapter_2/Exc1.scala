package chapter_2

import chapter_2.auxiliary._

object Exc1 extends App {

  val x = chapter_2.auxiliary.List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case chapter_2.auxiliary.Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  println(x)

}
