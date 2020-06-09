package io.ghostbuster91.the.red.book.strictness

import java.time.Instant

import io.ghostbuster91.the.red.book
import io.ghostbuster91.the.red.book.strictness.Stream.unfold
import io.ghostbuster91.the.red.book.{List, ListEx, Option}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

object Strictness {}

sealed trait Stream[+A] {
  def toList: List[A] = {
    this match {
      case Empty => List.empty[A]
      case Cons(head, tail) =>
        ListEx.append(List(head()), tail().toList)
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(head, tail) if n > 0 => Cons(head, () => tail().take(n - 1))
      case _                         => Stream.empty
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case other                  => other
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(head, tail) if p(head()) =>
        Cons(head, () => tail().takeWhile(p))
      case _ => Stream.empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(head, tail) =>
        f(head(), tail().foldRight(z)(f))
      case Empty => z
    }
  }

  def exist(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => b && p(a))
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])(
      (a, b) =>
        if (p(a)) {
          Stream.cons(a, b)
        } else {
          Stream.empty
      }
    )
  }

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])(
      (h, t) =>
        if (f(h)) Stream.cons(h, t)
        else Stream.empty
    )

  def headOption(): Option[A] = {
    foldRight(Option.none[A])((a, _) => Option.some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])(
      (a, acc) =>
        if (p(a)) {
          Stream.cons(a, acc)
        } else acc
    )
  }

  def append[B >: A](b: Stream[B]): Stream[B] = {
    this match {
      case Empty            => b
      case Cons(head, tail) => Stream.cons[B](head(), tail().append(b))
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  def map_unfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Empty            => Option.none[(Stream[A], B)]
      case Cons(head, tail) => Option.some(tail() -> f(head()))
    }
  }

  def take_unfold(n: Int): Stream[A] = {
    unfold(n -> this) {
      case (n, Cons(head, tail)) if n > 0 =>
        Option.some(n - 1 -> tail() -> head())
      case _ => Option.none
    }
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(head, tail) if p(head()) => Option.some(tail() -> head())
      case _                             => Option.none
    }
  }

  def zipWith_unfold[B](bs: Stream[B])(f: (A, B) => B): Stream[B] = {
    unfold(this -> bs) {
      case (Cons(ahead, atail), Cons(bhead, btail)) =>
        Option.some((atail() -> btail()) -> f(ahead(), bhead()))
      case (Empty, Empty) => Option.none
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this -> s2) {
      case (Cons(ahead, atail), Cons(bhead, btail)) =>
        Option.some(
          (atail() -> btail()) -> (Option.some(ahead()) -> Option.some(bhead()))
        )
      case (Empty, Cons(bhead, btail)) =>
        Option.some((Empty -> btail()) -> (Option.none -> Option.some(bhead())))
      case (Cons(ahead, atail), Empty) =>
        Option.some((atail() -> Empty) -> (Option.some(ahead()) -> Option.none))
      case (Empty, Empty) => Option.none
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case c @ Cons(head, tail) => Option.some(tail() -> c)
      case _                    => Option.none
    }
  }

  def tails_scanRight: Stream[Stream[A]] = {
    scanRight(Stream.empty[A])((a, b) => Stream.cons(a, b))
  }

  def scanRight[B](initial: B)(f: (A, => B) => B): Stream[B] = {
    unfold(this) {
      case c @ Cons(head, tail) =>
        Option.some(tail() -> c.foldRight(initial)(f))
      case _ => Option.none
    }
  }

  def smartScanRight[B](initial: B)(f: (A, => B) => B): Stream[B] = {
    //    1 + ( 2 + ( 3
    def go(ss: Stream[A]): (Stream[B], B) = {
      ss match {
        case Cons(head, tail) =>
          val (t, v) = go(tail())
          lazy val fv = f(head(), v)
          Stream.cons(fv, t) -> fv
        case Empty => Stream(initial) -> initial
      }
    }

    go(this)._1
  }

  def smartScanRight_book[B](initial: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(Stream(initial) -> initial) { (a, b) =>
      lazy val b0 = b
      val fv = f(a, b0._2)
      Stream.cons(fv, b0._1) -> fv
    }._1
  }
}
case object Empty extends Stream[Nothing]
case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def constant[A](v: A): Stream[A] = {
    lazy val r: Stream[A] = Cons(() => v, () => r)
    r
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def cons[A](hd: => A, td: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = td
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def fib(): Stream[BigInt] = {
    def go(n: BigInt, n1: BigInt): Stream[BigInt] = {
      Stream.cons(n, go(n1, n + n1))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(S, A)]): Stream[A] = {
    f(z) match {
      case book.Some((ns, na)) => Stream.cons(na, unfold(ns)(f))
      case book.None           => Stream.empty
    }
  }

  def unfoldConstant[A](v: A): Stream[A] = {
    unfold[A, Unit](())(s => Option.some(s -> v))
  }

  def unfoldFrom(v: Int): Stream[Int] = {
    unfold[Int, Int](v)(s => Option.some(s + 1, s))
  }

  def unfoldFib(): Stream[BigInt] = {
    unfold[BigInt, (BigInt, BigInt)]((0, 1))(
      s => Option.some((s._2 -> (s._1 + s._2)) -> s._1)
    )
  }

  def hasSubseq[A](as: Stream[A], bs: Stream[A]): Boolean = {
    unfold(as) {
      case Empty                => Option.none
      case s @ Cons(head, tail) => Option.some(tail() -> s)
    }.exist(startWith(_, bs))
  }

  def hasSubseq2[A](as: Stream[A], bs: Stream[A]): Boolean = {
    as.tails exist (startWith(_, bs))
  }

  private def startWith[A](as: Stream[A], bs: Stream[A]) = {
    as.zipAll(bs)
      .takeWhile {
        case (_, book.Some(_)) => true
        case _                 => false
      }
      .forAll {
        case (v1, v2) => v1 == v2
      }
  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).take(2).toList)
    println(Stream(1, 2, 3).drop(2).toList)
    println(Stream("a", "a", "a", "b").takeWhile(_ == "a").toList)
    println(
      Stream
        .cons("a", Stream.cons({
          println("ok")
          "a"
        }, Stream.cons("b", { println("not ok"); Stream("b") })))
        .takeWhile(_ == "a")
        .toList
    )
    println("=========================")
    println(
      Stream
        .cons("a", Stream.cons({
          println("ok")
          "a"
        }, Stream.cons("b", { println("not ok"); Stream("b") })))
        .takeWhileFoldRight(_ == "a")
        .toList
    )
    println("=========================")

    println("===========")
    println(c1.takeWhile(_ % 2 == 0).toList)
    println("===========")
    val value = c1
      .takeWhileFoldRight(_ % 2 == 0)
    println("result")
    println(value.toList)
    println("===========")
    val value2 = c1
      .takeWhile_1(_ % 2 == 0)
    println("result")
    println(value2.toList)
    println(Stream(2, 4, 6).forAll(_ % 2 == 0))
    println(Stream(2, 3, 6).forAll(_ % 2 == 0))
    println(Stream.empty.headOption())
    println(Cons(() => 1, () => { println("asd"); Stream(1) }).headOption())

    println(Stream(1, 2, 3, 4).map(_ + 1).toList)
    println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
    println(Stream(1, 2, 3, 4).append(Stream(3, 4, 5, 6)).toList)
    println(Stream(1, 2, 3, 4).flatMap(s => Stream(s, s, s)).toList)

    println("3333")
    println(Stream.constant(5).drop(100).take(1).toList)
    println(Stream.unfoldConstant(5).drop(100).take(1).toList)
    println(Stream.from(10).take(11).toList)
    println(Stream.unfoldFrom(10).take(11).toList)
    println(Stream.unfoldFib().drop(11).take(1).toList)
    println(Stream.fib().drop(11).take(1).toList)
    println(
      Stream
        .unfold[String, Int](1)(
          i =>
            if (i < 10) Option.some(i + 1 -> s"A$i")
            else Option.none
        )
        .toList
    )
    println("maj")
    println(Stream(1, 2, 3, 4).map_unfold(_ + 1).toList)
    println(Stream(1, 2, 3, 4).take_unfold(2).toList)
    println(Stream(1, 2, 3, 4).take(2).toList)
    println(c1.takeWhile_unfold(_ < 5).toList)
    println(
      Stream
        .unfoldFrom(0)
        .take(10)
        .zipWith_unfold(Stream.unfoldConstant(10).take(10))(_ + _)
        .toList
    )
    println(
      Stream
        .unfoldFrom(0)
        .take(20)
        .zipAll(Stream.unfoldConstant(10).take(10))
        .toList
    )

    println(
      Stream
        .unfoldFrom(0)
        .zipAll(Stream.unfoldConstant(10))
        .take(20)
        .toList
    )
    println("hasSub")
    println(
      Stream
        .hasSubseq(
          Stream
            .constant(1)
            .take(1)
            .append(Stream.unfoldConstant(10)),
          Stream.constant(1).take(1).append(Stream.constant(10).take(3))
        )
    )
    println(Stream.fib().drop(100).take(3).toList)

    println(
      Stream.hasSubseq(
        Stream.fib(),
        Stream(
          BigInt("354224848179261915075"),
          BigInt("573147844013817084101"),
          BigInt("927372692193078999176")
        )
      )
    )
//
    println(ListEx.map(Stream.unfoldFrom(0).take(10).tails.toList)(_.toList))

    println(
      Stream.hasSubseq2(
        Stream.fib(),
        Stream(
          BigInt("354224848179261915075"),
          BigInt("573147844013817084101"),
          BigInt("927372692193078999176")
        )
      )
    )
    println("scanRight")
    println(ListEx.map(Stream(1, 2, 3).tails.toList)(_.toList))
    println(ListEx.map(Stream(1, 2, 3).tails_scanRight.toList)(_.toList))
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
    println(Stream(1, 2, 3).smartScanRight(0)(_ + _).toList)
    println("debug smart")
    var counter = 0
    val function: (Int, => Int) => Int = { (a, b) =>
      println(s"computing $a + $b")
      counter = counter + 1;
      a + b
    }
    val l1 = Stream.constant(10).take(10).smartScanRight(0)(function).toList
    println(l1)
    println(s"counter $counter")
    println("Debug book")
    counter = 0
    val l2 =
      Stream.constant(10).take(10).smartScanRight_book(0)(function).toList
    println(counter)
    println(l2)
//    println("debug normal")
//    counter = 0
//    val l2 = Stream.constant(10).take(10).foldRight(0)(function)
//    println(l2)
//    println(s"counter $counter")
  }

  private def c1 = {
    Stream
      .cons(2, Stream.cons(5, {
        try {
          throw new RuntimeException("kasper")
        } catch {
          case e: Throwable =>
            e.printStackTrace();
            Thread.sleep(100)
            println("not ok");
            Stream(7)
        }
      }))
  }

}
