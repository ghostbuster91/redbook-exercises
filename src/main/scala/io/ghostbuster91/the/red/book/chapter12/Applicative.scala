package io.ghostbuster91.the.red.book.chapter12

import io.ghostbuster91.the.red.book.chapter11.Monads.Functor
import io.ghostbuster91.the.red.book.chapter3.monoids.Monoid

trait Applicative[F[_]] extends Functor[F] { outer =>
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab) { (a, f) => f(a) }
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }

  def map2Apply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fun1: F[A => (A, B)] =
      map[B, A => (A, B)](fb)((b: B) => (a: A) => (a, b))
    val fPair: F[(A, B)] = apply(fun1)(fa)
    map[(A, B), C](fPair)(f.tupled)

  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((a, b) => (a, b))
  }

  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    new Applicative[
      ({
        type f[x] = (F[x], G[x])
      })#f
    ] {
      override def unit[A](a: => A): (F[A], G[A]) = (outer.unit(a), G.unit(a))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
          f: (A, B) => C
      ): (F[C], G[C]) = {
        (outer.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }
  }

  def compse[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = new Applicative[
    ({
      type f[x] = F[G[x]]
    })#f
  ] {
    override def unit[A](a: => A): F[G[A]] = outer.unit(G.unit(a))

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
        f: (A, B) => C
    ): F[G[C]] = {
      outer.map2(fa, fb) { case (a, b) => G.map2(a, b)(f) }
    }
  }
}

object Applicative {
  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) = {
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero

      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(
          f: (A, B) => C
      ): Const[M, C] = {
        M.op(fa, fb)
      }
    }
  }
}
