package io.ghostbuster91.the.red.book.chapter13

import io.ghostbuster91.the.red.book.chapter11.Monads

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = {
    flatMap(f.andThen(Return[F, B](_)))
  }
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = {
    FlatMap[F, A, B](this, f)
  }
}
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
    extends Free[F, B]

object Free {
  def freeMonad[F[_]]: Monads.Monad[({ type f[x] = Free[F, x] })#f] =
    new Monads.Monad[({ type f[x] = Free[F, x] })#f] {
      override def flatMap[A, B](
          fa: Free[F, A]
      )(f: A => Free[F, B]): Free[F, B] = {
        fa.flatMap(f)
      }
      def unit[A](a: => A): Free[F, A] = Return(a)
    }

  def runTrampoline[A](a: Free[Function0, A]): A = {
    a match {
      case Return(a)  => a
      case Suspend(s) => s()
      case FlatMap(s, f) =>
        s match {
          case Return(a)       => runTrampoline(f(a))
          case Suspend(s)      => runTrampoline(f(s()))
          case FlatMap(s2, f2) => runTrampoline(s2.flatMap(f2).flatMap(f))
        }
    }
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monads.Monad[F]): F[A] = {
    a match {
      case Return(a)  => F.unit(a)
      case Suspend(s) => s
      case FlatMap(s, f) =>
        s match {
          case Return(a)       => run(f(a))
          case Suspend(s2)     => F.flatMap(s2)(a => run(f(a)))
          case FlatMap(s2, f2) => run(s2.flatMap(f2).flatMap(f))
        }
    }
  }

  def main(args: Array[String]): Unit = {}
}
