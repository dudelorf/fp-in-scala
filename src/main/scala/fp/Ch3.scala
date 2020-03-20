package fp

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil              => 1.0
    case Cons(0.0, _)     => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Ch3 {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil              => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](newHead: A, l: List[A]): List[A] = l match {
    case Nil              => Cons(newHead, Nil)
    case Cons(head, tail) => Cons(newHead, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil           => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if p(head) => dropWhile(tail, p)
      case _                           => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(head, tail)   => Cons(head, init(tail))
    }

  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]) = foldLeft(l, 0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((lt, _) => lt + 1)

  def reverse[A](l: List[A]) =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = 
    List.foldRight(reverse(as), z)((a, b) => f(b, a))
    
  def foldRight2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldLeft(reverse(as), z)(f)
    
  def append[A](as1: List[A], as2: List[A]): List[A] =
    List.foldRight(as1, Nil:List[A])(Cons(_,_))
    
  def concatenate[A](as: List[List[A]]): List[A] =
    List.foldRight(as, Nil:List[A])(append)
    
  def addOne(as: List[Int]): List[Int] =
    List.foldRight(as, Nil:List[Int])((a, acc) => Cons(a + 1, acc))
  
  def toStrings(as: List[Double]): List[String] = 
    List.foldRight(as, Nil:List[String])((a, acc) => Cons(a.toString, acc))
    
  def map[A,B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil:List[B])((a, acc) => Cons(f(a), acc))
    
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRight(as, Nil:List[A])((a, acc) => if(f(a)) Cons(a, acc) else acc)
    
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))
    
  def filterVflatMap[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if(p(a)) List(a) else Nil)
    
  def zip(as1: List[Int], as2: List[Int]): List[Int] = ???
  
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = ???
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???


  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r)
    case Leaf(_) => 1
  }
  
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }
  
  def depth(t: Tree[Int]): Int = t match {
    case Branch(l, r) => (depth(l) max depth(r)) + 1
    case Leaf(v) => 0
  }
  
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }
  
  def fold = ???
  
}

