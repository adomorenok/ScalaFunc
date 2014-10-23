package chapter_4.auxiliary


sealed trait MyStream[+A] { self =>
  def uncons: Option[(A, MyStream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toListRec: List[A] = uncons match {
    case Some((h, t)) => h :: t.toList
    case None => List()
  }

  def toList: List[A] = {
    @annotation.tailrec
    def aux(init: MyStream[A], rest: List[A]): List[A] = init.uncons match {
      case Some((h, t)) => aux(t, h :: rest)
      case _ => rest
    }
    aux(self, List()).reverse
  }

  def take(n: Int): MyStream[A] = {

    def aux(init: MyStream[A], rest: MyStream[A], aggr: Int): MyStream[A] =
      if (aggr >= n) rest
      else init.uncons match {
        case Some((h, t)) => MyStream.cons(h, aux(t, rest, aggr + 1))
        case _ => rest
      }
    aux(self, MyStream(), 0)
  }


  def takeWhile(f: A => Boolean): MyStream[A] = {

    def aux(init: MyStream[A], rest: MyStream[A]): MyStream[A] = init.uncons match {
      case Some((h, t)) => if(f(h)) MyStream.cons(h, aux(t, rest)) else rest
      case _ => rest
    }
    aux(self, MyStream())
  }


// tail recursive analogues, though working twice as long, due to reversing

//  def take(n: Int): MyStream[A] = {
//    @annotation.tailrec
//    def aux(init: MyStream[A], rest: MyStream[A], aggr: Int): MyStream[A] =
//      if (aggr >= n) rest
//      else init.uncons match {
//        case Some((h, t)) => aux(t, MyStream.cons(h, rest), aggr + 1)
//        case _ => rest
//      }
//    aux(self, MyStream(), 0).reverse
//  }
//  def takeWhile(f: A => Boolean): MyStream[A] = {
//    @annotation.tailrec
//    def aux(init: MyStream[A], rest: MyStream[A]): MyStream[A] = init.uncons match {
//      case Some((h, t)) => if(f(h)) aux(t, MyStream.cons(h, rest)) else rest
//      case _ => rest
//    }
//    aux(self, MyStream()).reverse
//  }
//  private def reverse(): MyStream[A] = {
//    @annotation.tailrec
//    def aux(init: MyStream[A], rest: MyStream[A]): MyStream[A] = init.uncons match {
//      case Some((h, t)) => aux(t, MyStream.cons(h, rest))
//      case _ => rest
//    }
//
//    aux(self, MyStream())
//  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some((h, t)) => f(h, t.foldRight(z)(f))
    case None => z
  }
}

object MyStream {

  def empty[A]: MyStream[A] =
    new MyStream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] =
    new MyStream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
