package fp

import scala.concurrent.duration.TimeUnit

object Ch7 {
//  def sum(ints: IndexedSeq[Int]): Par[Int] =
//    if (ints.size <= 1)
//      Par.unit(ints.headOption getOrElse 0)
//    else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
//    }
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] {
  def call: A
}
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(evenIfRunning: Boolean): Boolean = true
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def lazyUnit[A](a: A) = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    def balancedSeq(ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
      if (ps.isEmpty) unit(Vector())
      else if (ps.length == 1) map(ps.head)(Vector(_))
      else {
        val (l, r) = ps.splitAt(ps.length / 2)
        map2(balancedSeq(l), balancedSeq(r))(_ ++ _)
      }
    }

    map(balancedSeq(ps.toIndexedSeq))(_.toList)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map asyncF((a: A) => if(f(a)) List(a) else List())

    map(sequence(pars))(_.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val num = run(es)(n).get
      run(es)(choices(num))

    }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val c = run(es)(pa).get
      run(es)(choices(c))
    }

  def choiceNviaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def chooseViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if(_) t else f)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get)

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))
}