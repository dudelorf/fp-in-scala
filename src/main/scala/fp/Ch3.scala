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
    
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

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
      case Cons(head, tail) => Cons(head, init(tail))
    }
    
  def length[A](as: List[A]): Int =
    List.foldRight(as, 0)((_, acc) => acc + 1)
    
  
}
