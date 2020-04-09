package fp

import scala.collection.immutable.List
import scala.collection.immutable.Nil
import fp.Ch5.Stream._

object Ch5 {
  sealed trait Stream[+A] {
  
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty      => Nil
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case _                    => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _                      => empty
    }
    
    def forAll(p: A => Boolean): Boolean =
      this.foldRight(true)((a, b) => p(a) && b)
      
    def takeWhile2(p: A => Boolean): Stream[A] =
      this.foldRight(empty[A])((h, t) =>
        if(p(h)) cons(h, t)
        else empty
      )
      
    def headOptionFr: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))
      
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => 
        if(p(h)) cons(h, t)
        else t
    )

    def append[B >: A](s: => Stream[B]): Stream[A] = ???

    def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  }
  
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
