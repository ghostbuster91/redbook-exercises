package io.ghostbuster91.the.red.book.chapter8

import io.ghostbuster91.the.red.book._
import io.ghostbuster91.the.red.book.nonblocking.NonBlocking
import io.ghostbuster91.the.red.book.nonblocking.NonBlocking.Par
import io.ghostbuster91.the.red.book.state.StateExc.State
import io.ghostbuster91.the.red.book.state.{RNG, SimpleRNG, StateExc}

import java.util.concurrent.Executors

sealed trait Result {
  def isFalsified: Boolean
  def &&(r: Result): Result = {
    (this, r) match {
      case (Passed, Passed) => Passed
      case (f: Falsified, f2: Falsified) =>
        Falsified(f.failure + f2.failure, f.success + f2.success)
      case (f: Falsified, Passed) => f
      case (Passed, f: Falsified) => f
    }
  }
  def ||(r: Result): Result = {
    (this, r) match {
      case (Passed, Passed) => Passed
      case (f: Falsified, f2: Falsified) =>
        Falsified(f.failure + f2.failure, f.success + f2.success)
      case (f: Falsified, Passed) => Passed
      case (Passed, f: Falsified) => Passed
    }
  }
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: String, success: Int) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class SGen[A](forSize: Int => Gen[A])
object SGen {
  def listOfN[A](g: Gen[A]): SGen[List[A]] = {
    SGen(s => Gen.listOfN(s, g))
  }

  def listOfAtLeastOne[A](g: Gen[A]): SGen[List[A]] = {
    SGen(s => Gen.listOfN(s max 1, g))
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(s => Gen.listOfN(s, this))
  }

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = {
    flatMap(a => Gen.unit(f(a)))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    flatMap(a => g.map(b => f(a, b)))
  }

  def **[B](g: Gen[B]): Gen[(A, B)] = {
    map2(g)((_, _))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State { rng => StateExc.choice(start, stopExclusive).run(rng) })
  }

  def unit[A](a: A): Gen[A] = Gen(State(rng => (a, rng)))
  def boolean: Gen[Boolean] = Gen(StateExc.int.map(_ % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(StateExc.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap {
      case true  => g1
      case false => g2
    }
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(StateExc.int.map(_ % 100).flatMap { v =>
      if (v < g1._2 * 100) {
        g1._1.sample
      } else {
        g2._1.sample
      }
    })
  }
}

object Testing {
  type TestCases = Int
  type MaxSize = Int
  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = {
      Prop((m, t, r) => run(m, t, r).&&(p.run(m, t, r)))
    }
    def ||(p: Prop): Prop = {
      Prop((m, t, r) => run(m, t, r).||(p.run(m, t, r)))
    }
  }

  object Prop {
    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) {
        Proved
      } else {
        Falsified("()", 0)
      }
    }

    def checkPar[A](p: => Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props =
        strictness.Stream
          .from(0)
          .take((n min max) + 1)
          .map(i => forAll(g(i))(f))
      val prop = ListEx.foldLeft(
        props.map(_.run(max, casesPerSize, rng)).toList,
        Passed: Result
      )(_ && _)
      prop
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = {
    Prop((m, tc, rng) => {
      randomStream(a)(rng)
        .zipAll(strictness.Stream.from(0))
        .take(tc)
        .map { case (Some(a), Some(i)) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(e.getMessage, i)
          }
        }
        .filter(_.isFalsified)
        .headOption()
        .getOrElse(Passed)
    })
  }
  def randomStream[A](as: Gen[A])(rng: RNG): strictness.Stream[A] = {
    strictness.Stream.unfold(rng)(r => Some(as.sample.run(r).swap))
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newSingleThreadExecutor()) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(S ** g) { case (s, a) => NonBlocking.run(s)(f(a)) }
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(SGen.listOfAtLeastOne(smallInt)) { a =>
    val max = a.max
    ListEx.length(ListEx.filter(a)(i => i > max)) == 0
  }

  val ES = Executors.newFixedThreadPool(4)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = {
    NonBlocking.map2(p, p2)(_ == _)
  }

  val p2 = Prop.checkPar {
    val p = NonBlocking.map(NonBlocking.unit(1))(_ + 1)
    val p2 = NonBlocking.unit(2)
    equal(p, p2)
  }

  def main(args: Array[String]): Unit = {
    println(maxProp.run(100, 100, new SimpleRNG(0L)))
    println(p2.run(100, 100, new SimpleRNG(0L)))
  }
}
