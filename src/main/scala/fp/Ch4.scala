package fp

object Ch4 {

  sealed trait Option[+A] {
    def mean(xs: Seq[Int]) =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def map[B](f: A => B): Option[B] = this match {
      case None      => None
      case Some(get) => Some(f(get))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None      => default
      case Some(get) => get
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
    // a flatMap (aa => b map (bb => f(aa, bb)))

    def sequence[A](a: FPList[Option[A]]): Option[FPList[A]] = a match {
      case FPNil => Some(FPNil)
      case FPCons(h, t) =>
        for {
          hh <- h
          tt <- sequence(t)
        } yield FPCons(hh, tt)
      // case Cons(h, t) => h flatMap (hh => sequence(t) map (tt => Cons(hh, tt)))
    }

    def traverse[A, B](a: FPList[A])(f: A => Option[B]): Option[FPList[B]] =
      a match {
        case FPNil              => Some(FPNil)
        case FPCons(head, tail) => map2(f(head), traverse(tail)(f))(FPCons(_, _))
      }

    def sequenceViaTraverse[A](a: FPList[Option[A]]): Option[FPList[A]] =
      traverse(a)(identity)
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(value)  => Left(value)
      case Right(value) => Right(f(value))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = b match {
      case Left(value)  => b
      case Right(value) => Right(value)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
      
    def sequence[E, A](es: FPList[Either[E, A]]): Either[E, FPList[A]] =
      traverse(es)(identity)
    
    def traverse[E, A, B](as: FPList[A])(f: A => Either[E, B]): Either[E, FPList[B]] = as match {
      case FPNil => Right(FPNil)
      case FPCons(head, tail) => f(head).map2(traverse(tail)(f))(FPCons(_, _))
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}
