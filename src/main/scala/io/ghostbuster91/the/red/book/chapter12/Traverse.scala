package io.ghostbuster91.the.red.book.chapter12

import io.ghostbuster91.the.red.book.chapter11.Monads
import io.ghostbuster91.the.red.book.chapter11.Monads.Functor
import io.ghostbuster91.the.red.book.chapter12.Applicative.{
  Const,
  monoidApplicative
}
import io.ghostbuster91.the.red.book.chapter12.Traverse.Id
import io.ghostbuster91.the.red.book.chapter3.monoids.Monoid
import io.ghostbuster91.the.red.book.state.StateExc
import io.ghostbuster91.the.red.book.state.StateExc.State

trait Traverse[F[_]] extends Functor[F] { outer =>
  def traverse[G[_], A, B](
      fa: F[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = {
    traverse(fga)(identity)
  }

  val idMonad = new Monads.Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = {
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )
  }
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(
      Monads.stateMonad2[S]
    )
  }

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = {
    mapAccum(fa, 0) { (a, s) =>
      ((a, s), s + 1)
    }._1
  }

  def toList[A](fa: F[A]): List[A] = {
    mapAccum(fa, List.empty[A]) { (a, s) =>
      ((), a :: s)
    }._2.reverse
  }

  def reverse[A: Monoid](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse) { (a, s) =>
      (s.head, s.tail)
    }._1
  }

  def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B = {
    mapAccum(as, z) { (a, s) =>
      ((), f(s, a))
    }._2
  }

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa) { a =>
      for {
        s1 <- StateExc.get[S]
        (b, s2) = f(a, s1)
        _ <- StateExc.set(s2)
      } yield b
    }.run(s)

  def fuse[G[_], H[_], A, B](fa: F[A])(
      f: A => G[B],
      g: A => H[B]
  )(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    type f[x] = (G[x], H[x])
    val gh: Applicative[f] = G.product(H)
    val fg: A => f[B] = (a: A) => (f(a), g(a))
    traverse(fa)(fg)(gh)
  }

  def compose[G[_]](implicit
      G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] =
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[M[_], A, B](
          fa: F[G[A]]
      )(f: A => M[B])(implicit M: Applicative[M]) = {
        outer.traverse(fa) { ga: G[A] => G.traverse(ga)(f)(M) }
      }
    }

  def composeM[F[_], G[_]](
      F: Monads.Monad[F],
      G: Monads.Monad[G],
      T: Traverse[G]
  ): Monads.Monad[({ type f[x] = F[G[x]] })#f] =
    new Monads.Monad[({ type f[x] = F[G[x]] })#f] {
      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        val a = F.flatMap(fa) { a: G[A] =>
          val gf: G[F[G[B]]] = G.map(a)(f)
          val fg: F[G[G[B]]] = T.sequence(gf)(F)
          val ff: F[G[B]] = F.map(fg)(gg => G.join(gg))
          ff
        }
        a
      }

      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }

}

object Traverse {
  type Id[A] = A

  implicit val idInstance: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }
}
