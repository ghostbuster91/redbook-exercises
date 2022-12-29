package io.ghostbuster91.the.red.book.chapter13

import io.ghostbuster91.the.red.book.chapter11.Monads
import io.ghostbuster91.the.red.book.concurrency.Par.Par
import scala.annotation.tailrec

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

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Free {
  type ~>[F[_], G[_]] = Translate[F, G]
  def freeMonad[F[_]]: Monads.Monad[({ type f[x] = Free[F, x] })#f] =
    new Monads.Monad[({ type f[x] = Free[F, x] })#f] {
      override def flatMap[A, B](
          fa: Free[F, A]
      )(f: A => Free[F, B]): Free[F, B] = {
        fa.flatMap(f)
      }
      def unit[A](a: => A): Free[F, A] = Return(a)
    }

  @tailrec
  def step[F[_], A](fa: Free[F, A]): Free[F, A] = {
    fa match {
      case FlatMap(Return(x), f)     => step(f(x))
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
      case _                         => fa
    }
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = {
    a match {
      case Return(a)  => a
      case Suspend(s) => s()
      case FlatMap(s, f) =>
        s match {
          case Return(a)  => runTrampoline(f(a))
          case Suspend(s) => runTrampoline(f(s()))
          case FlatMap(s2, f2) =>
            runTrampoline(s2.flatMap(a => f2(a).flatMap(f)))
        }
    }
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monads.Monad[F]): F[A] = {
    step(a) match {
      case Return(a)  => F.unit(a)
      case Suspend(s) => s
      case FlatMap(s, f) =>
        s match {
          case Suspend(s2) => F.flatMap(s2)(a => run(f(a)))
          case _           => sys.error("step eliminated case")
        }
    }
  }

  def runFree[F[_], G[_], A](a: Free[F, A])(t: F ~> G)(implicit
      G: Monads.Monad[G]
  ): G[A] = {
    step(a) match {
      case Return(a)  => G.unit(a)
      case Suspend(s) => t(s)
      case FlatMap(s, f) =>
        s match {
          case Suspend(s2) => G.flatMap(t(s2))(a => runFree(f(a))(t))
          case _           => sys.error("step eliminated case")
        }
    }
  }

  type Tailrec[A] = Free[Function0, A]

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend { fg(a) }
    }
    runFree(f)(t)(freeMonad[G])
  }

}

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}
object Console {
  val consoleToFunction0 = new Translate[Console, Function0] {
    def apply[A](a: Console[A]) = a.toThunk
  }

  implicit val function0Monad = new Monads.Monad[Function0] {
    override def flatMap[A, B](fa: () => A)(f: A => (() => B)): () => B = () =>
      f(fa())()
    override def unit[A](a: => A): () => A = () => a
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = {
    Free.runFree[Console, Function0, A](a)(consoleToFunction0)
  }
}
