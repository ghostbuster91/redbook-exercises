package io.ghostbuster91.the.red.book.chapter14

import io.ghostbuster91.the.red.book.strictness._
import io.ghostbuster91.the.red.book.chapter11.Monads
import io.ghostbuster91.the.red.book.chapter13.Free._
import io.ghostbuster91.the.red.book.chapter14.Process1._
import scala.io

sealed trait Process1[I, O] { self =>
  def apply(s: Stream[I]): Stream[O] = {
    this match {
      case Emit(head, tail) => Stream.cons(head, tail(s))
      case Halt()           => Stream()
      case Await(recv) =>
        s match {
          case Cons(head, tail) => recv(Some(head()))(tail())
          case xs               => recv(None)(xs) // empty
        }

    }
  }

  def repeat: Process1[I, O] = {
    def go(p: Process1[I, O]): Process1[I, O] = {
      p match {
        case Emit(head, tail) => Emit(head, go(tail))
        case Halt()           => go(this)
        case Await(recv) =>
          Await {
            case None        => recv(None)
            case Some(value) => go(recv(Some(value)))
          }

      }
    }
    go(this)
  }

  def |>[O2](p2: Process1[O, O2]): Process1[I, O2] = {
    p2 match {
      case Halt()     => Halt()
      case Emit(h, t) => Emit(h, self |> t)
      case Await(f) =>
        self match {
          case Emit(head, tail) => tail |> f(Some(head))
          case Halt()           => Halt() |> f(None)
          case Await(recv)      => Await((i: Option[I]) => recv(i) |> p2)
        }
    }
  }

  def map[O2](f: O => O2): Process1[I, O2] = {
    this |> Process1.lift(f)
  }

  def ++(p2: Process1[I, O]): Process1[I, O] = {
    this match {
      case Emit(head, tail) => Emit(head, tail ++ p2)
      case Halt()           => p2
      case Await(recv)      => Await(i => recv(i) ++ p2)
    }
  }

  def flatMap[O2](f: O => Process1[I, O2]): Process1[I, O2] = {
    this match {
      case Emit(head, tail) => f(head) ++ tail.flatMap(f)
      case Halt()           => Halt()
      case Await(recv)      => Await(i => recv(i).flatMap(f))
    }
  }

  def zipWithIndex: Process1[I, (O, Int)] = {
    this |> Process1.zipWithIndex
  }

  def zipWith[O2](p: Process1[I, O2]): Process1[I, (O, O2)] = {
    this match {
      case Emit(head, tail) =>
        p match {
          case Emit(head2, tail2) =>
            Emit((head, head2), tail zipWith tail2)
          case Halt()      => Halt()
          case Await(recv) => Await(i => this zipWith recv(i))
        }
      case Halt() => Halt()
      case Await(recv) =>
        p match {
          case Emit(head, tail) => Await(i => recv(i) zipWith p)
          case Halt()           => Halt()
          case Await(recv2)     => Await(i => recv(i) zipWith recv2(i))
        }
    }
  }

  def zipWithIndex2: Process1[I, (O, Int)] = this zipWith Process1.count[I]
}

object Process1 {

  case class Emit[I, O](head: O, tail: Process1[I, O] = Halt[I, O]())
      extends Process1[I, O]

  case class Halt[I, O]() extends Process1[I, O]

  case class Await[I, O](recv: Option[I] => Process1[I, O])
      extends Process1[I, O]

  def liftOne[I, O](f: I => O): Process1[I, O] = {
    Await {
      case Some(value) => Emit(f(value))
      case None        => Halt()
    }
  }
  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process1[I, I] = {
    Await[I, I] {
      case Some(value) if (p(value)) => Emit(value)
      case _                         => Halt()
    }.repeat
  }

  def sum[T: Numeric]: Process1[T, T] = {
    loop(Numeric[T].zero) { case (i, s) =>
      val ns = Numeric[T].plus(i, s)
      (ns, ns)
    }
  }

  def take[I](n: Int): Process1[I, I] = {
    def go(counter: Int): Process1[I, I] = {
      Await {
        case Some(value) if counter > 0 =>
          Emit(value, go(counter - 1))
        case _ => Halt()
      }
    }
    go(n)
  }
  def drop[I](n: Int): Process1[I, I] = {
    def go(counter: Int): Process1[I, I] = {
      Await {
        case Some(value) if counter > 0 =>
          go(counter - 1)
        case Some(value) =>
          Emit(value, go(counter))
        case None => Halt()
      }
    }
    go(n)
  }

  def takeWhile[I](f: I => Boolean): Process1[I, I] = {
    def go(): Process1[I, I] = {
      Await {
        case Some(value) if f(value) =>
          Emit(value, go())
        case _ => Halt()
      }
    }
    go()
  }

  def dropWhile[I](f: I => Boolean): Process1[I, I] = {
    def go(dropping: Boolean): Process1[I, I] = {
      if (dropping) {
        Await {
          case Some(value) => go(f(value))
          case _           => Halt()
        }
      } else {
        Await {
          case Some(value) => Emit(value, go(false))
          case _           => Halt()
        }
      }
    }
    go(true)
  }

  def count[I]: Process1[I, Int] = {
    loop(0) { case (i, s) =>
      val nS = s + 1
      (nS, nS)
    }
  }

  def mean: Process1[Double, Double] = {
    loop((0, 0.0)) { case (i, (count, sum)) =>
      val nSum = sum + i
      val nCount = count + 1
      (nSum / nCount) -> (nCount, nSum)
    }
  }

  def loop[I, O, S](zero: S)(f: (I, S) => (O, S)): Process1[I, O] = {
    def go(state: S): Process1[I, O] = {
      Await {
        case Some(value) =>
          val (o, s) = f(value, state)
          Emit(o, go(s))
        case _ => Halt()
      }
    }
    go(zero)
  }

  def monad[I]: Monads.Monad[({ type f[x] = Process1[I, x] })#f] =
    new Monads.Monad[({ type f[x] = Process1[I, x] })#f] {
      def unit[A](a: => A): Process1[I, A] = Emit(a)
      def flatMap[A, B](fa: Process1[I, A])(
          f: A => Process1[I, B]
      ): Process1[I, B] = {
        fa.flatMap(f)
      }
    }

  def zipWithIndex[I]: Process1[I, (I, Int)] = {
    loop(0) { case (i, s) =>
      val ns = s + 1
      ((i, ns), ns)
    }
  }

  def mean2: Process1[Double, Double] = {
    Process1
      .count[Double]
      .zipWith(Process1.sum[Double])
      .map { case (c, s) =>
        s / c
      }
  }

  def exists[I](p: I => Boolean) = {
    Process1.loop[I, Boolean, Boolean](false) { case (i, s) =>
      if (s) {
        (true, true)
      } else {
        val r = p(i)
        (r, r)
      }
    }
  }
}

object Play {

  def processFile[A, B](f: java.io.File, p: Process1[String, A], z: B)(
      g: (B, A) => B
  ): IO[B] = IO {
    def go(ss: Iterator[String], cur: Process1[String, A], acc: B): B = {
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next =
            if (ss.hasNext) recv(Some(ss.next))
            else recv(None)
          go(ss, next, acc)
        case Emit(head, tail) => go(ss, tail, g(acc, head))
      }
    }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }

  def main(args: Array[String]): Unit = {
    val even = Process1.filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1, 2, 3, 4, 5, 6))

    println(evens.toList)

    val s = Process1.sum[Int].apply(Stream(1, 2, 3, 4, 5, 6))
    println(s.toList)

    val takeP = Process1.take[Int](3).apply(Stream(7, 1, 2, 3, 4, 5, 6))
    println(takeP.toList)

    val dropP = Process1.drop[Int](3).apply(Stream(7, 1, 2, 3, 4, 5, 6))
    println(dropP.toList)

    val numbersD = Stream[Double](1, 2, 3, 4)
    val numbers = Stream[Int](1, 2, 3, 4)

    println("mean")
    println(Process1.mean(numbersD).toList)
    println(Process1.mean2(numbersD).toList)
    println(Process1.count(numbersD).toList)

    val fused = Process1.filter[Int](_ % 2 == 0) |> Process1.lift(_ + 1)
    println(fused(numbers).toList)

    println("zipWithIndex")
    println(Process1.zipWithIndex(numbers).toList)
    println(Process1.take(10).zipWithIndex(numbers).toList)
    println(Process1.take(10).zipWithIndex2(numbers).toList)
  }
}
