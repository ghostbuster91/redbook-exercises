package io.ghostbuster91.the.red.book.chapter11

import io.ghostbuster91.the.red.book.Option
import io.ghostbuster91.the.red.book.chapter8.Gen
import io.ghostbuster91.the.red.book.state.StateExc.{State, main}

object Monads {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
      flatMap(fa)(a => map(fb)(b => f(a, b)))
    }

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
      la match {
        case ::(head, next) =>
          val headF = flatMap(unit(head))(a => f(a))
          flatMap(headF)(b => map(traverse(next)(f))(bs => b :: bs))
        case Nil => unit(List.empty[B])
      }
    }

    def sequence[A](la: List[F[A]]): F[List[A]] = traverse(la)(identity)

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      map2(ma, replicateM(n - 1, ma)) { case (a, b) => a :: b }
    }
    def fileterM[A](fs: List[A])(f: A => F[Boolean]): F[List[A]] = {
      map(
        traverse(fs)(a =>
          map(f(a)) { fa =>
            if (fa) {
              List(a)
            } else {
              List.empty
            }
          }
        )
      )(_.flatten)
    }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = { a =>
      flatMap(f(a))(b => g(b))
    }

    def flatMapCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
      def zz(u: Unit) = fa
      compose(zz, f).apply(())
    }

    def join[A](mma: F[F[A]]): F[A] = {
      flatMap(mma)(a => a)
    }

    def flatMapJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = {
      join(map(fa)(f))
    }
  }

  def optMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option.some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }

  def genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
  }

  def stateMonad[S] = {
    type StateS[A] = State[S, A]

    new Monad[StateS] {
      override def unit[A](a: => A): StateS[A] = State.unit(a)

      override def flatMap[A, B](fa: StateS[A])(f: A => StateS[B]): StateS[B] =
        fa.flatMap(f)
    }
  }

  def stateMonad2[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    override def unit[A](a: => A): State[S, A] = ???

    override def flatMap[A, B](fa: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] = ???
  }

  def main(args: Array[String]): Unit = {
    val value: List[Option[Int]] =
      List(Option.some(1), Option.some(3), Option.none)
    val value1: Option[List[Int]] = optMonad.traverse(value)(a => a)

    val F = Reader.readerMonad[String]
    val r1 = F.unit(1)
    F.flatMap(r1)(_ => r1)
    val value2: Reader[String, List[Int]] = F.sequence(List(r1, r1))
    println(value1)
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
      override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      override def flatMap[A, B](fa: Reader[R, A])(
          f: A => Reader[R, B]
      ): Reader[R, B] = {
        Reader(r => f(fa.run(r)).run(r))
      }
    }
  }
}
