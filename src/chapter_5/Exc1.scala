package chapter_5

import chapter_5.auxiliary.RND

object Exc1 extends App {
  val simple = RND.simple(4L)
  print(" NEXT INT ")
  println(simple.nextInt)
  print(" RANDOM PAIR ")
  println(RND.randomPair(simple))
  print(" POSITIVE INT ")
  println(RND.positiveInt(simple))
  print(" RANDOM LIST ")
  println(RND.ints(5)(simple))
  print(" INT VALUE ")
  println(RND.int(simple))
  print(" UNIT ")
  println(RND.unit(3)(simple))
}
