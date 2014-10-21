package chapter_4.auxiliary


sealed trait MyStream[+A] {
  def uncons: Option[(A, MyStream[A])]
  def isEmpty: Boolean = mycons.isEmpty
}

object MyStream {

  def empty[A]: Stream[A] =
    new MyStream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] =
    new MyStream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}