package fp

import fp.Ch6.Rand

trait RNG {
  def nextInt: (Int, RNG)
}

object Ch6 {

  type Rand[+A] = RNG => (A, RNG)

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

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))

  def ints2(counts: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(counts)(int))

  def flatMap[A,B](s: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = s(rng)
      g(a)(r)
    }

  def mapFm[A, B](s: Rand[A])(f: A => B): Rand[B] = ???

  def map2Fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

}
