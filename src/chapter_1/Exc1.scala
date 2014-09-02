package chapter_1

object Exc1 extends App {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int): Boolean = {
      if(index + 1 >= as.length) true
      else if(gt(as(index), as(index+1))) go(index + 1)
      else false
    }
    go(0)
  }

  val arr = Array(1,2,3,4,5)
  println(isSorted(arr, (x: Int, y: Int) => x < y))

}
