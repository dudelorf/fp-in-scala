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

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h) append t)

    def constant[A](a: A): Stream[A] = {
      lazy val s: Stream[A] = Cons(() => a, () => s)
      s
    }
    
    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))
      
    def fibs: Stream[Int] = {
      def go(p0: Int, p1: Int): Stream[Int] = cons(p1, go(p1, p0 + p1))
      go(0, 1)
    }
      
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => empty
      }
      
    def fibsUnfold: Stream[Int] =
      unfold((0, 1))((s: (Int, Int)) => Some(s._2, (s._2, s._1 + s._2)))

    def fromUnfold(n: Int): Stream[Int] =
      unfold(n)((s: Int) => Some(s, s + 1))
    
    def constantUnfold[A](a: A): Stream[A] =
      unfold(a)(_ => Some(a, a))

    def onesUnfold: Stream[Int] =
      unfold(1)(_ => Some(1, 1))
      
    def mapUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(head, tail) => Some((f(head()), tail()))
        case _ => None
      }
    
    def takeUnfold(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), 1) => Some(h(), (empty, 0))
        case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
        case _ => None
      }

    def takeWhileUnfold(p: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }
    
    def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s2)) {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
        case (Empty, Cons(hb, tb)) => Some(( (None, Some(hb())), (Empty, Empty) ))
        case (Cons(ha, ta), Empty) => Some(( (Some(ha()), None), (Empty, Empty) ))
        case (Empty, Empty) => None
      }
      
    def startsWith[A](s2: Stream[A]): Boolean =
      zipAll(s2).takeWhile(_._2.isDefined).forAll {
        case (Some(h1), Some(h2)) => h1 == h2
      }

    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s => Some((s, s drop 1))
      } append Stream(empty)
    
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
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
