package chapter_2

import chapter_2.auxiliary.{Zero, Cons, MyList}

//checking MyList methods
object Exc2 extends App {

  val x = MyList(1, 2, 3, 4, 5)
  val y = MyList(MyList(1, 2, 3), MyList(4, 5, 6))
  val z = MyList(1.0, 2.0, 3.0, 4.0, 5.0)
  print(" TAIL ")
  println(MyList.tail(x))
  print(" DROP ")
  println(MyList.drop(x, 3))
  print(" DROP WHILE ")
  println(MyList.dropWhile(x)( _ <= 3))
  println(MyList.dropWhile(x)( (x) => { x < 3} ))
  print(" INIT ")
  println(MyList.init(x))
  print(" FOLD RIGHT ")
  println(MyList.foldRight(x, Zero: MyList[Int])(Cons(_,_)))
  print(" LENGTH ")
  println(MyList.length(x))
  print(" FOLD LEFT SUM ")
  println(MyList.lsum(x))
  print(" FOLD LEFT PRODUCT ")
  println(MyList.lproduct(MyList(1.0, 2.0, 3.0)))
  print(" REVERSE ")
  println(MyList.reverse(x))
  print(" FLATTEN ")
  println(MyList.flatten(y))
  print(" MAP BY ONE ")
  println(MyList.mapByOne(x))
  print(" MAP BY STRING ")
  println(MyList.mapByString(z))
  print(" MAP ")
  println(MyList.map(x)(_ * 100))
  print(" HAS SUBSEQUENCE ")
  println( MyList.hasSubsequence(x, MyList(1, 2, 3, 4, 5)))
  println( MyList.hasSubsequence(x, MyList(3, 4)))

}
