package io.ghostbuster91.the.red.book.state

import io.ghostbuster91.the.red.book._

import scala.util.Random

trait RNG {
  def nextInt: (Int, RNG)
}

class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = new SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    n -> nextRng
  }
}

object StateExc {

  def main(args: Array[String]): Unit = {
    println("start")
    callNonNegativeInt
    callNonNegativeInt
    callNonNegativeInt
    callNonNegativeInt
    val g = new SimpleRNG(123)
    val tuple = ints(12)(g)
    println(tuple._1)
    println(ints(12)(tuple._2))

    println(sequence(List(int, int, int))(g))
    println(sequenceTailRec(List(int, int, int))(g))

    println(nonNegativeLessThan(1000)(g)._1)

    println()
    println()
    println("machine")
    println()
    val buyCandy = List(Coin, Turn)
    val value = simulateMachine(
      ListEx.concatMany(List(buyCandy, buyCandy, buyCandy, buyCandy))
    )
    println(value.run(Machine(locked = true, candies = 5, coins = 10)))
  }

  private def callNonNegativeInt = {
    val g = new SimpleRNG(Random.nextLong())
    println(nonNegativeInt(g)._1)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue - Double.MinPositiveValue)(
      rng
    )
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = both(int, State(double))(rng)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = both(State(double), int)(rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    if (int < 0) {
      Math.abs(int + 1) -> rng2
    } else {
      int -> rng2
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(acc: List[Int],
           countInternal: Int,
           rngInternal: RNG): (List[Int], RNG) = {
      if (countInternal > 0) {
        val nextInt = rngInternal.nextInt
        go(Cons(nextInt._1, acc), countInternal - 1, nextInt._2)
      } else {
        acc -> rngInternal
      }
    }
    go(List.empty, count, rng)
  }

  def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State {
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
  }

  val int: Rand[Int] = State(_.nextInt)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)(_ -> _)

  def unit[A](a: A): Rand[A] = State { rng =>
    a -> rng
  }

  type Rand[+A] = State[RNG, A]
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = State { rng =>
    fs match {
      case Nil => List.empty[A] -> rng
      case Cons(head, tail) =>
        val (v, newRng) = head(rng)
        val tuple = sequence(tail)(newRng)
        Cons(v, tuple._1) -> tuple._2
    }
  }

  def sequenceTailRec[A](fs: List[Rand[A]]): Rand[List[A]] = State { rng =>
    @scala.annotation.tailrec
    def go(in: List[Rand[A]], out: (List[A], RNG)): (List[A], RNG) = {
      in match {
        case Nil => out
        case Cons(head, tail) =>
          val tuple = head(out._2)
          go(tail, Cons(tuple._1, out._1) -> tuple._2)
      }
    }
    go(fs, List.empty -> rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State { rng =>
    val tuple = f(rng)
    g(tuple._1)(tuple._2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(State(nonNegativeInt)) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def map_flat[A, B](s: Rand[A])(f: A => B): Rand[B] = State { rng =>
    flatMap(s) { sa =>
      unit(f(sa))
    }(rng)
  }

  def map2_flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    State { rng =>
      flatMap(ra) { a =>
        flatMap(rb) { b =>
          unit(f(a, b))
        }
      }(rng)
    }

  case class State[S, +A](run: S => (A, S)) {
    def apply(s: S): (A, S) = run(s)

    def map[B](f: A => B): State[S, B] = State { s =>
      val tuple = run(s)
      f(tuple._1) -> tuple._2
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val tuple = run(s)
      f(tuple._1)(tuple._2)
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State { rng =>
      a -> rng
    }
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State {
      rng =>
        @scala.annotation.tailrec
        def go(in: List[State[S, A]], out: (List[A], S)): (List[A], S) = {
          in match {
            case Nil => out
            case Cons(head, tail) =>
              val tuple = head(out._2)
              go(tail, Cons(tuple._1, out._1) -> tuple._2)
          }
        }
        go(fs, List.empty -> rng)
    }

    def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(
      f: (A, B) => C
    ): State[S, C] = State { rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => () -> s)

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(ListEx.map(inputs)(process2))
      s <- get
    } yield s.coins -> s.candies
  }

  def process2(input: Input): State[Machine, Unit] = {
    input match {
      case Coin =>
        for {
          s <- get
          _ <- set(s.copy(locked = false, coins = s.coins + 1))
        } yield ()
      case Turn =>
        for {
          s <- get
          _ <- if (!s.locked) {
            set(s.copy(locked = true, candies = s.candies - 1))
          } else {
            set(s)
          }
        } yield ()
    }
  }
}
