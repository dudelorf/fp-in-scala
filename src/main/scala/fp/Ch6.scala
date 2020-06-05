package fp

import fp.Ch6.Rand
import fp.State.set

import scala.util.Random

trait RNG {
  def nextInt: (Int, RNG)
}

object Ch6 {

  type Rand[+A] = State[RNG, A]

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def int: Rand[Int] = _.nextInt

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = nonNegativeInt(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(counts: Int)(rng: RNG): (List[Int], RNG) =
    if(counts > 0) {
      val (i, r1) = rng.nextInt
      val (is, r2) = ints(counts - 1)(r1)
      (i :: is, r2)
    } else
      (Nil, rng)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double2 =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def ints2(counts: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(counts)(int))

  def flatMap[A,B](s: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      g(a)(r)
    }

  def mapFm[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  type State[S,+A] = S => (A,S)
}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s)
    })
}

object State {
  def unit[S,A](a: A): State[S, A] =
    State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  val p: Input => State[Machine, Unit] = (i: Input) => {
    modify[Machine](update(i))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(i => modify[Machine](update(i))))
    .flatMap(_ => get[Machine].map(s => (s.coins, s.candies)))

  simulateMachine(List[Input]()).run(Machine(true, 1, 1))
}

