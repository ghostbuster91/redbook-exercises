package io.ghostbuster91.the.red.book.chapter12

import io.ghostbuster91.the.red.book.chapter11.Monads.Functor
import io.ghostbuster91.the.red.book.chapter3.monoids.Monoid

trait Applicative[F[_]] extends Functor[F] { outer =>
  def unit[A](a: => A): F[A]

  def applyMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab) { (a, f) => f(a) }
  }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab)((a, f) => f(a))
  }

  def map[A, B](f: A => B)(fa: F[A]): F[B] = {
    apply(unit(f))(fa)
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fun1: F[A => (A, B)] =
      map[B, A => (A, B)]((b: B) => (a: A) => (a, b))(fb)
    val fPair: F[(A, B)] = apply(fun1)(fa)
    map[(A, B), C](f.tupled)(fPair)
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

      override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = {
        (outer.map(f)(fa._1), G.map(f)(fa._2))
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

    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = {
      outer.map[G[A], G[B]](G.map[A, B](f)(_))(fa)
    }
  }
}

object Applicative {
  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) = {
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero

      override def map[A, B](fa: Const[M, A])(f: A => B): Const[M, B] =
        M.op(fa, fa)
    }
  }
}
