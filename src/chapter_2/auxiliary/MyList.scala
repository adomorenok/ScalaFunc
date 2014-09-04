package chapter_2.auxiliary

sealed trait MyList[+A]

case object Zero extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Zero => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Zero => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Zero
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: MyList[A]): MyList[A] =
    l match {
      case Zero => Zero
      case Cons(x, xs) => xs
    }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    @annotation.tailrec
    def aux(counter: Int, cl: MyList[A]): MyList[A] =
      cl match {
        case Zero => Zero
        case Cons(x, xs) => if (counter < n) aux(counter + 1, xs) else Cons(x, xs)
      }

    aux(0, l)
  }

  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = {
    @annotation.tailrec
    def aux(counter: Int, cl: MyList[A]): MyList[A] =
      cl match {
        case Zero => Zero
        case Cons(x, xs) => if (f(x)) aux(counter + 1, xs) else Cons(x, xs)
      }

    aux(0, l)
  }

  def setHead[A](l: MyList[A], h: A): MyList[A] =
    l match {
      case Zero => Zero
      case Cons(_, xs) => Cons(h, xs)
    }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case Zero => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def init[A](l: MyList[A]): MyList[A] = {
    def aux(aggr: MyList[A], initial: MyList[A]): MyList[A] = initial match {
      case Zero => Zero
      case Cons(_, Zero) => aggr
      case Cons(x, xs) => aux(append(aggr, Cons(x, Zero)), xs)
    }
    aux(Zero, l)
  }

//  val example = Cons(1, Cons(2, Cons(3, Zero)))
//  val example2 = MyList(1,2,3)
//  val total = sum(example)
}
