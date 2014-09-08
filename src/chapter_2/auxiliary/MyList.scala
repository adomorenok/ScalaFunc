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

  def rappend[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r)(Cons(_,_))

  def init[A](l: MyList[A]): MyList[A] = {
    @annotation.tailrec
    def aux(aggr: MyList[A], initial: MyList[A]): MyList[A] = initial match {
      case Zero => Zero
      case Cons(_, Zero) => aggr
      case Cons(x, xs) => aux(append(aggr, Cons(x, Zero)), xs)
    }
    aux(Zero, l)
  }

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    l match {
      case Zero => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def flatMap[A](l: MyList[MyList[A]]): MyList[A] = foldRight(l, MyList[A]())((r, l) => rappend(r, l))

  @annotation.tailrec
  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B =
    l match {
      case Zero => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def lsum(ints: MyList[Int]): Int = foldLeft[Int, Int](ints, 0)(_ + _)

  def lproduct(doubles: MyList[Double]): Double = foldLeft[Double, Double](doubles, 1.0) (_ * _)



  def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, MyList[A]())((r, l) => Cons(l, r))

  def foldRightViaFoldLeft_1[A,B](l: MyList[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: MyList[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b :B) => b)((a, g) => b => g(f(b,a)))(z)

  def length[A](l: MyList[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }





//  val example = Cons(1, Cons(2, Cons(3, Zero)))
//  val example2 = MyList(1,2,3)
//  val total = sum(example)
}
