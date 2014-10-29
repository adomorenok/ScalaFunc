package chapter_5.auxiliary

/**
 * Created by Anton.Nekrasov
 * 10/28/2014 11:50
 */
trait RND {
  def nextInt: (Int, RND)
}

object RND {

  def simple(seed: Long): RND = new RND {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }

  }

  def positiveInt(rnd: RND): (Int, RND) = {
    val (i, r) = rnd.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rnd: RND): (Double, RND) = {
    val (i, r) = positiveInt(rnd)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rnd: RND): ((Int,Double), RND) = {
    val (i, r1) = rnd.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rnd: RND): ((Double,Int), RND) = {
    val ((i, d), r) = intDouble(rnd)
    ((d, i), r)
  }

  def double3(rnd: RND): ((Double,Double,Double), RND) = {
    val (d1, r1) = double(rnd)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rnd: RND): (List[Int], RND) = {
    def aux(lth: Int, lst: List[Int])(r: RND): (List[Int], RND) = if(lth < count) {
      val (i, r1) = r.nextInt
      aux(lth + 1, i :: lst)(r1)
    } else (lst, r)

    aux(0, List[Int]())(rnd)
  }

  def randomPair(rng: RND): ((Int,Int), RND) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  type Rand[+A] = RND => (A, RND)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rnd => (a, rnd)

//  def positiveMax(n: Int): Rand[Int] =
//    map(_.nextInt)(x => if(x > 0 && x < n) x else n)

  def _double: Rand[Double] =
    map(positiveInt)(_/(Int.MaxValue.toDouble + 1))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rnd => {
      val (a, rnd1) = f(rnd)
      g(a)(rnd1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(positiveInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (a, r1) = ra(rnd)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}


