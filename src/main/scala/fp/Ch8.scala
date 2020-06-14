package fp

import fp.Prop._

object Ch8 {
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop (
    (m, n, rng) => {
      val casesPerSize = (n + (m - 1)) / m
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min m) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop( (max, _, rng) => p.run(max, casesPerSize, rng)))
            .toList.reduce(_ && _)
      prop.run(m, n, rng)
    }
  )
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    Prop (
      (_, n, rng) => randomStream(a)(rng).zip(LazyList.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(e.getMessage, i)}
      }.find(_.isFalsified).getOrElse(Passed)
    )

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, t, rng) =>
      run(m, t, rng) match {
        case Passed => p.run(m, t, rng)
        case failed => failed
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, t, rng) =>
      run(m, t, rng) match {
        case Falsified(f, s) => p.run(m, t, rng)
        case passed => passed
      }
    }
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(Ch6.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(choose(0, 1).sample.map(_ == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if(_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_).forSize(n) }
    }
    SGen(g2)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen { Gen.listOfN(_, g) }
  }
}
