package io.ghostbuster91.the.red.book.chapter3.monoids

import io.ghostbuster91.the.red.book.nonblocking.NonBlocking
import io.ghostbuster91.the.red.book.nonblocking.NonBlocking.{Par, run}

import java.util.concurrent.Executors

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object M {
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    override val zero: A => A = identity
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero) { case (acc, item) => m.op(acc, f(item)) }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldMap(
      as,
      endoMonoid[B]
    )(a => b => f(a, b))(z)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size > 2) {
      val (left, right) = v.splitAt(v.size / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    } else if (v.size == 2) {
      m.op(f(v.head), f(v(1)))
    } else {
      f(v.head)
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] =
        NonBlocking.map2(a1, a2)(m.op)

      override val zero: Par[A] = NonBlocking.unit(m.zero)
    }
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM = par(m)
    if (v.size > 2) {
      val (left, right) = v.splitAt(v.size / 2)
      parM.op(parFoldMap(left, m)(f), parFoldMap(right, m)(f))
    } else if (v.size == 2) {
      parM.op(NonBlocking.lazyUnit(f(v.head)), NonBlocking.lazyUnit(f(v(1))))
    } else {
      NonBlocking.lazyUnit(f(v.head))
    }
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 ++ a2

    override val zero: String = ""
  }

  def isOrdered(is: IndexedSeq[Int]): Boolean = {
    val res = foldMapV(
      is,
      new Monoid[(Int, Int)] {
        override def op(a1: (Int, Int), a2: (Int, Int)): (Int, Int) = {
          if (a1._1 < a2._1) {
            (a2._1, a1._2 + a2._2)
          } else {
            (a2._1, 1)
          }
        }

        override val zero: (Int, Int) = (0, 0)
      }
    )(a => (a, 0))
    res._2 == 0
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (s1: Stub, s2: Stub) => Stub(s1.chars ++ s2.chars)
        case (p1: Part, p2: Part) =>
          val middle = p1.rStub ++ p2.lStub
          val add = if (middle.isEmpty) 0 else 1
          Part(p1.lStub, p1.words + p2.words + add, p2.rStub)
        case (s1: Stub, p2: Part) =>
          Part(s1.chars ++ p2.lStub, p2.words, p2.rStub)
        case (p1: Part, s2: Stub) =>
          Part(p1.lStub, p1.words, p1.rStub ++ s2.chars)
      }
    }

    override val zero: WC = Stub("")
  }

  def splitCount(string: String): WC = {
    if (string.length >= 2) {
      val (left, right) = string.splitAt(string.length / 2)
      wcMonoid.op(splitCount(left), splitCount(right))
    } else {
      if (string == " ") {
        Part("", 0, "")
      } else {
        Stub(string)
      }
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = {
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      }

      override def zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = { a =>
      B.op(a1(a), a2(a))
    }

    override def zero: A => B = _ => B.zero
  }

  def main(args: Array[String]): Unit = {
    println(foldRight(List("a", "b", "c"), "")(_ ++ _))
    println(List("a", "b", "c").foldRight("")(_ ++ _))
    println(foldMapV(Vector("a", "b", "c"), stringMonoid)(identity))

    val service = Executors.newFixedThreadPool(2)
    val x = run(service)(
      parFoldMap(Vector("a", "b", "c", "d", "e", "f"), stringMonoid)(identity)
    )
    println(x)
    service.shutdown()

    println(isOrdered(Vector(1, 2, 3, 4, 15)))
    println(isOrdered(Vector(1, 2, 6, 4, 5)))
    println(splitCount("lorem ipsum dolor sit ament, "))
  }
}
