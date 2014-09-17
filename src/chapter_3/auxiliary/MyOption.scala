package chapter_3.auxiliary

sealed trait MyOption[+A] { self =>
  def map[B](f: => A => B): MyOption[B] = self match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = self match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

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

case class MySome[A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]
