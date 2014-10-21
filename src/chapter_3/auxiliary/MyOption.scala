package chapter_3.auxiliary

import java.util.regex.{PatternSyntaxException, Pattern}

sealed trait MyOption[+A] { self =>
  def map[B](f: => A => B): MyOption[B] = self match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = self match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

  def flatMap2[B](f: A => MyOption[B]): MyOption[B] = self map(f) getOrElse MyNone

  def getOrElse[B >: A](default: => B): B = self match {
    case MyNone => default
    case MySome(a) => a
  }

  def orElse[B >: A](default: => MyOption[B]): MyOption[B] = self match {
    case MyNone => default
    case MySome(a) => MySome(a)
  }

  def filter(f: A => Boolean): MyOption[A] = self match {
    case MyNone => MyNone
    case MySome(a) => if(f(a)) MySome(a) else MyNone
  }

}


object MyOption {

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    for {
      un <- a
      dos <- b
    } yield f(un, dos)

  def map21[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def pattern(s: String): MyOption[Pattern] =
    try {
      MySome(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => MyNone
    }

  def mkMatcher(pat: String): MyOption[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)


  def bothMatch_2(pat1: String, pat2: String, s: String): MyOption[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = a match {
    case Nil => MySome(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => MySome(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

}

case class MySome[A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]