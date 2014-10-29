package chapter_4

import chapter_4.auxiliary.{MyStream => ms}

object Exc1 extends App {
  def pair(i: => Int) = (i, i)

  ms.cons(1, ms(1, ms.empty))
  pair {println("hello"); 41 + 1}
  val x = ms(1, 3, 4, 6)


  print(" TO LIST ")
  println(x.toList)
  print(" TAKE ")
  println(x.take(2).toList)
  print(" TAKE VIA UNFOLD ")
  println(x.takeUnf(2).toList)
  print(" TAKE WHILE ")
  println(x.takeWhile(_ < 4 ).toList)
  print(" TAKE WHILE VIA UNFOLD ")
  println(x.takeWhileUnf(_ < 4).toList)
  print(" EXISTS ")
  println(x.exists(_ == 3))
  print(" TAKE WHILE THROUGH FOLD RIGHT ")
  println(x.takeWhileFR(_ < 4).toList)
  print(" FILTER ")
  println(x.filter(_ != 3).toList)
  print(" APPEND ")
  println(x.appendL(9).toList)

  val ones = ms.constant(1)

  print(" INFINITE STREAM TAKE WHILE ")
  println(ones.takeWhile(_ == 1))

  val from3 = ms.fromUnf(3)
  print(" INFINITE STREAM FROM 3 TAKE 5 ")
  println(from3.take(5).toList)

  val fbn = ms.fibs

  print(" INFINITE STREAM FIBONACCI TAKE 8 ")
  println(fbn.take(8).toList)

  val cstUnf = ms.constantUnf(3)
  print(" INFINITE STREAM CONSTANT 3 TAKE 8 ")
  println(cstUnf.take(8).toList)

  print(" MAP VIA UNFOLD ")
  println(x.mapUnf(_ * 100).toList)

  print(" ZIP WITH ")
  println(x.zipWith(ms(2, 4))(_ + _).toList)

  print(" ZIP ALL ")
  println(x.zipWithAll(ms(1, 3))((x, y) => (x.getOrElse(0) == y.getOrElse(0))).toList)

  print(" STARTS WITH ")
  println(ms.startsWith(x, ms(1,3,4,5)))

  print(" TAIL ")
  println(x.tails.toList.map(_.toList))



}
