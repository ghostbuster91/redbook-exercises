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

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](
      fa: F[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = {
    sequence(map(fa)(f)(Traverse.idInstance))
  }

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = {
    traverse(fga)(identity)
  }

  def map[A, B](fa: F[A])(f: A => B)(implicit id: Applicative[Id]): F[B] = {
    traverse[Id, A, B](fa)(f)
  }

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

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] = {
    traverseS(ta) { a =>
      for {
        i <- StateExc.get[Int]
        _ <- StateExc.set(i + 1)
      } yield (a, i)
    }.run(0)._1
  }

  def toList[A](fa: F[A]): List[A] = {
    traverseS(fa) { a =>
      for {
        as <- StateExc.get[List[A]]
        _ <- StateExc.set(a :: as)
      } yield ()
    }.run(Nil)._2
  }
}

object Traverse {
  type Id[A] = A

  implicit val idInstance: Applicative[Id] = new Applicative[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }
}
