package fp

sealed trait FPList[+A]
case object FPNil extends FPList[Nothing]
case class FPCons[+A](head: A, tail: FPList[A]) extends FPList[A]

object FPList {
  def sum(ints: FPList[Int]): Int = ints match {
    case FPNil              => 0
    case FPCons(head, tail) => head + sum(tail)
  }

  def product(ds: FPList[Double]): Double = ds match {
    case FPNil              => 1.0
    case FPCons(0.0, _)     => 0.0
    case FPCons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): FPList[A] =
    if (as.isEmpty) FPNil
    else FPCons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: FPList[A], z: B)(f: (A, B) => B): B =
    as match {
      case FPNil         => z
      case FPCons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Ch3 {

  def tail[A](l: FPList[A]): FPList[A] = l match {
    case FPNil              => FPNil
    case FPCons(head, tail) => tail
  }

  def setHead[A](newHead: A, l: FPList[A]): FPList[A] = l match {
    case FPNil              => FPCons(newHead, FPNil)
    case FPCons(head, tail) => FPCons(newHead, tail)
  }

  def drop[A](l: FPList[A], n: Int): FPList[A] =
    if (n <= 0) l
    else
      l match {
        case FPNil           => FPNil
        case FPCons(_, tail) => drop(tail, n - 1)
      }

  def dropWhile[A](l: FPList[A], p: A => Boolean): FPList[A] =
    l match {
      case FPCons(head, tail) if p(head) => dropWhile(tail, p)
      case _                           => l
    }

  def init[A](l: FPList[A]): FPList[A] =
    l match {
      case FPNil | FPCons(_, FPNil) => FPNil
      case FPCons(head, tail)   => FPCons(head, init(tail))
    }

  def length[A](as: FPList[A]): Int =
    FPList.foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: FPList[A], z: B)(f: (B, A) => B): B = as match {
    case FPNil              => z
    case FPCons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum(l: FPList[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: FPList[Int]) = foldLeft(l, 0)(_ * _)

  def length2[A](l: FPList[A]): Int = foldLeft(l, 0)((lt, _) => lt + 1)

  def reverse[A](l: FPList[A]) =
    foldLeft(l, FPNil: FPList[A])((acc, h) => FPCons(h, acc))

  def foldLeft2[A, B](as: FPList[A], z: B)(f: (B, A) => B): B =
    FPList.foldRight(reverse(as), z)((a, b) => f(b, a))

  def foldRight2[A, B](as: FPList[A], z: B)(f: (B, A) => B): B =
    foldLeft(reverse(as), z)(f)

  def append[A](as1: FPList[A], as2: FPList[A]): FPList[A] =
    FPList.foldRight(as1, FPNil: FPList[A])(FPCons(_, _))

  def concatenate[A](as: FPList[FPList[A]]): FPList[A] =
    FPList.foldRight(as, FPNil: FPList[A])(append)

  def addOne(as: FPList[Int]): FPList[Int] =
    FPList.foldRight(as, FPNil: FPList[Int])((a, acc) => FPCons(a + 1, acc))

  def toStrings(as: FPList[Double]): FPList[String] =
    FPList.foldRight(as, FPNil: FPList[String])((a, acc) => FPCons(a.toString, acc))

  def map[A, B](as: FPList[A])(f: A => B): FPList[B] =
    FPList.foldRight(as, FPNil: FPList[B])((a, acc) => FPCons(f(a), acc))

  def filter[A](as: FPList[A])(f: A => Boolean): FPList[A] =
    FPList.foldRight(as, FPNil: FPList[A])((a, acc) =>
      if (f(a)) FPCons(a, acc) else acc
    )

  def flatMap[A, B](as: FPList[A])(f: A => FPList[B]): FPList[B] =
    concatenate(map(as)(f))

  def filterVflatMap[A](as: FPList[A])(p: A => Boolean): FPList[A] =
    flatMap(as)(a => if (p(a)) FPList(a) else FPNil)

  def zip(as1: FPList[Int], as2: FPList[Int]): FPList[Int] = {
    @annotation.tailrec
    def loop(acc: FPList[Int], a: FPList[Int], b: FPList[Int]): FPList[Int] =
      (a, b) match {
        case (FPCons(ha, ta), FPCons(hb, tb)) => loop(FPCons(ha + hb, acc), ta, tb)
        case _                            => acc
      }

    loop(FPNil, as1, as2)
  }

  def zipWith[A,B](as1: FPList[A], as2: FPList[A])(f: (A, A) => B): FPList[B] = {
   @annotation.tailrec
    def loop(acc: FPList[B], a: FPList[A], b: FPList[A]): FPList[B] = (a, b) match {
      case(FPCons(ha, ta), FPCons(hb, tb)) => loop(FPCons(f(ha, hb), acc), ta, tb)
      case _ => acc
    }

    loop(FPNil: FPList[B], as1, as2)
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: FPList[A], sub: FPList[A]): Boolean = {
    @annotation.tailrec
    def startsWithSequence(l: FPList[A], s: FPList[A]): Boolean = (l, s) match {
      case (_, FPNil) => true
      case (FPCons(lh, lt), FPCons(sh, st)) if lh == sh => startsWithSequence(lt, st)
      case _ => false
    }

    sup match {
      case FPNil => false
      case _ if startsWithSequence(sup, sub) => true
      case FPCons(_, t) => hasSubsequence(t, sub)
    }
  }

  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r)
    case Leaf(_)      => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v)      => v
  }

  def depth(t: Tree[Int]): Int = t match {
    case Branch(l, r) => (depth(l) max depth(r)) + 1
    case Leaf(v)      => 0
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v)      => Leaf(f(v))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

}
