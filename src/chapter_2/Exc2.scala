package chapter_2

import chapter_2.auxiliary.MyList

//checking MyList methods
object Exc2 extends App {

  val x = MyList(1, 2, 3, 4, 5)
  println(MyList.tail(x))
  println(MyList.drop(x, 3))
  println(MyList.dropWhile(x)( _ <= 3))
//  println(MyList.dropWhile(x)( (x) => { x < 3} ))

  println(MyList.init(x))
}
