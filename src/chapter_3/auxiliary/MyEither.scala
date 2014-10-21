package chapter_3.auxiliary

sealed trait MyEither[+E, +A] { self =>

  def map[B](f: A => B): MyEither[E, B] = self match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = self match {
    case MyRight(a) => f(a)
    case MyLeft(b) => MyLeft(b)
  }

  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = self match {
    case MyRight(a) => MyRight(a)
    case MyLeft(e) => b
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      aa <- self
      bb <- b
    } yield f(aa, bb)

//  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =


}

object MyEither {
  def traverse[E,A,B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    es match {
      case Nil => MyRight(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[E,A](es: List[MyEither[E,A]]): MyEither[E,List[A]] =
    traverse(es)(x => x)
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]


