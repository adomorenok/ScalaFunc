package chapter_2.auxiliary

/**
 * Created by Anton.Nekrasov
 * 9/15/2014 17:58
 */
sealed trait MyTree[+A]

case class MyLeaf[+A](value: A) extends MyTree[A]
case class MyBranch[+A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  def size[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: MyTree[Int]): Int = t match {
    case MyLeaf(n) => n
    case MyBranch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 0
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case MyLeaf(a) => MyLeaf(f(a))
    case MyBranch(l, r) => MyBranch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: MyTree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case MyLeaf(a) => f(a)
    case MyBranch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: MyTree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: MyTree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: MyTree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](t: MyTree[A])(f: A => B): MyTree[B] =
    fold(t)(a => MyLeaf(f(a)): MyTree[B])(MyBranch(_,_))

}