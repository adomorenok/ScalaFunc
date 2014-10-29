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

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileFR(f: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A])((h, t) =>
      if(f(h)) MyStream.cons(h, t)
      else MyStream.empty)

  def map[B](f: A => B): MyStream[B] =
    foldRight(MyStream.empty[B])((h, t) => MyStream.cons(f(h), t))

  def filter(f: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A]) ((h, t) => if(f(h)) MyStream.cons(h, t) else t)

  def append[B>:A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((h, t) => MyStream.cons(h, t))

  def appendL[B>:A](s: => B): MyStream[B] =
    foldRight(MyStream.cons(s, MyStream.empty))((h, t) => MyStream.cons(h, t))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(MyStream.empty[B])((h, t) => f(h) append t)

  def mapUnf[B](f: A => B): MyStream[B] =
    MyStream.unfold(self) ( x => {
      x.uncons match {
        case Some((h, t)) => Some((f(h), t))
        case _ => None
      }
    })

  def takeUnf(n: Int): MyStream[A] =
    MyStream.unfold((self, 0)) {
      case (a, b) => if(b < n) a.uncons match {
        case Some((h, t)) => Some((h, (t, b + 1)))
        case _ => None
      } else None
    }

  def takeWhileUnf(f: A => Boolean): MyStream[A] =
    MyStream.unfold(self) ( x => {
      x.uncons match {
        case Some((h, t)) => if(f(h)) Some((h, t)) else None
        case _ => None
      }
    })

  def zipWith[B, C](s2: MyStream[B])(f: (A, B) => C): MyStream[C] =
    MyStream.unfold((self, s2)) {
      case (a: MyStream[A], b: MyStream[B]) =>
        a.uncons match {
          case Some((ha, ta)) => b.uncons match {
            case Some((hb, tb)) => Some((f(ha, hb), (ta, tb)))
            case _ => None
          }
          case _ => None
        }
    }

  def zip[B](s2: MyStream[B]): MyStream[(A,B)] =
    zipWith(s2)((_,_))

  def zipWithAll[B, C](s2: MyStream[B])(f: (Option[A], Option[B]) => C): MyStream[C] =
    MyStream.unfold((self, s2)) {
      case (a: MyStream[A], b: MyStream[B]) =>
        a.uncons match {
          case Some((ha, ta)) => b.uncons match {
            case Some((hb, tb)) => Some((f(Some(ha), Some(hb)), (ta, tb)))
            case _ => Some((f(Some(ha), None), (ta, MyStream.empty)))
          }
          case _ => b.uncons match {
            case Some((hb, tb)) => Some((f(None, Some(hb)), (MyStream.empty, tb)))
            case _ => None
          }
        }
    }

  def zipAll[B](s2: MyStream[B]): MyStream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def tails: MyStream[MyStream[A]] =
    MyStream.unfold(self)( x => {
      x.uncons match {
        case Some((h, t)) => Some((MyStream.cons(h, t), t))
        case _ => None
      }
    }).append(MyStream.empty)

  def hasSubsequence[A](s: MyStream[A]): Boolean =
    tails exists (x => MyStream.startsWith(x, s))

  def scanRight[B](z: B)(f: (A,=>B) => B): MyStream[B] =
    foldRight((z, MyStream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, MyStream.cons(b2, p._2))
    })._2
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

  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

  def from(n: Int): MyStream[Int] = cons(n, from(n+1))

  def fibs: MyStream[Int] = {
    def aux(prv: Int, nxt: Int): MyStream[Int] = cons(prv, aux(nxt, prv + nxt))
    aux(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  def fibsUnf: MyStream[Int] =
    unfold((0, 1)) {
      case (f0,f1) => Some(
        (f0, (f1, f0 + f1))
      )
    }

  def fromUnf(n: Int): MyStream[Int] =
    unfold(n)((x) => Some((x, x + 1)))

  def constantUnf(n: Int): MyStream[Int] =
    unfold(n)((x) => Some((x, x)))

  def onesUnf: MyStream[Int] = unfold(1)(x => Some((1, 1)))

  def startsWith[A](s1: MyStream[A], s2: MyStream[A]): Boolean =
    s1.zipWithAll(s2)(_ == _).forAll(_ != false)



}
