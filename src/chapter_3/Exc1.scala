package chapter_3

import chapter_3.auxiliary.{MyOption, MySome}

object Exc1 extends App {

  val o = MySome(1)
  print(" MAP ")
  println(o.map(_ + 1))
  print(" FLAT_MAP ")
  println(o.flatMap( x => MySome(x + 4) ))
  print(" GET_OR_ELSE ")
  println(o.getOrElse(0))
  print(" OR_ELSE ")
  println(o.orElse(MySome(0)))
  print(" FILTER ")
  println(o.filter( _ < 0))

  val p = MySome(2)
  println(MyOption.map2(o, p)(_ + _))
  println(MyOption.map21(o, p) (_ + _))

}
