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

  def head[A](l: MyList[A]): A = {
    l match {
      case Zero => sys.error("no head")
      case Cons(h, t) => h
    }
  }

  def isEmpty[A](l: MyList[A]): Boolean = l match {
      case Zero => true
      case Cons(h, t) => false
    }


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

  def mapByOne(l: MyList[Int]): MyList[Int] = l match {
    case Zero => l
    case Cons(h, t) => Cons(h + 1, mapByOne(t))
  }

  def mapByString(l: MyList[Double]): MyList[String] = l match {
    case Zero => MyList[String]()
    case Cons(h, t) => Cons(h.toString, mapByString(t))
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

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] =
    foldRight(l, MyList[B]())((lf, rt) => Cons(f(lf), rt))

  def flatMap[A,B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
    flatten(map(l)(f))

  def filterViaFlatMap[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(l)((a) => if(f(a)) MyList(a) else Zero)

  def filter[A](l: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(l, MyList[A]())((h, t) => if(f(h)) Cons(h, t) else t)

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    l match {
      case Zero => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def flatten[A](l: MyList[MyList[A]]): MyList[A] = foldRight(l, MyList[A]())((r, l) => rappend(r, l))

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

  def zipPlus[Int](l: MyList[Int], r: MyList[Int]): MyList[Int] = (l,r) match {
    case (_, Zero) => Zero
    case(Zero, _) => Zero
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1 + h2), zipPlus(t1, t2))
  }

  def zipWith[A,B,C](a: MyList[A], b: MyList[B])(f: (A,B) => C): MyList[C] = (a,b) match {
    case (Zero, _) => Zero
    case (_, Zero) => Zero
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](l: MyList[A], sub: MyList[A]): Boolean = {
    // if subsequence is empty
    if(isEmpty(sub)) return false

    //head of subsequence structure
    val hd = head(sub)

    //checking sequences equality
    //note: sequences are considered to be equal, if
    @annotation.tailrec
    def eqCheck(a: MyList[A], b: MyList[A]): Boolean = (a, b) match {
      case (_, Zero) => true
      case (Zero, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) eqCheck(t1, t2) else false
    }

    dropWhile(l)(_ == hd) match {
      case Zero => false
      case Cons(h, t) => {
        if(eqCheck(Cons(hd, Cons(h, t)), sub)) true else hasSubsequence(t, sub)
      }
    }
  }

//  val example = Cons(1, Cons(2, Cons(3, Zero)))
//  val example2 = MyList(1,2,3)
//  val total = sum(example)
}
