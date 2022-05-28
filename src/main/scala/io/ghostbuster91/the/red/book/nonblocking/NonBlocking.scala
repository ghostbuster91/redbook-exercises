package io.ghostbuster91.the.red.book.nonblocking

import java.util.concurrent.{
  Callable,
  CountDownLatch,
  ExecutorService,
  Executors
}
import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success, Try}

object NonBlocking {
  sealed trait Future[+A] {
    private[nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[Try[A]]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[Try[A]]()
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get().get
  }

  def unit[A](a: A): Par[A] = { es =>
    new Future[Try[A]] {
      override private[nonblocking] def apply(k: Try[A] => Unit): Unit = {
        k(Try(a))
      }
    }
  }

  def fork[A](a: => Par[A]): Par[A] = { es =>
    new Future[Try[A]] {
      override private[nonblocking] def apply(k: Try[A] => Unit): Unit = {
        println("beforeFork")
        eval(es) {
          Try(a(es)) match {
            case Failure(exception) => k.apply(Failure(exception))
            case Success(value)     => value.apply(k)
          }
        }
        println("afterFork")
      }
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { es =>
    new Future[Try[C]] {
      override private[nonblocking] def apply(k: Try[C] => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = new Actor[Either[Try[A], Try[B]]](es)(handler = {
          case Left(Success(valueA)) =>
            br match {
              case Some(valueB) =>
                println("getBA")
                eval(es)(k(Try(f(valueA, valueB))))
              case None =>
                println("getA")
                ar = Some(valueA)
            }
          case Right(Success(valueB)) =>
            ar match {
              case Some(valueA) =>
                println("getAB")
                eval(es)(k(Try(f(valueA, valueB))))
              case None =>
                println("getB")
                br = Some(valueB)
            }
          case Left(Failure(exception)) =>
            br match {
              case Some(valueB) =>
                println("getFBA")
                eval(es)(k(Failure(exception)))
              case None =>
                println("getFA")
            }

          case Right(Failure(exception)) =>
            ar match {
              case Some(valueA) =>
                println("getFAB")
                eval(es)(k(Failure(exception)))
              case None =>
                println("getFB")
            }
        })
        a(es)(aV => combiner ! Left(aV))
        b(es)(bV => combiner ! Right(bV))
      }
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case ::(head, next) =>
        map2(head, fork(sequence(next))) { (a, b) => a :: b }
      case Nil => unit(List.empty)
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    sequence(fbs)
  }

  def main(args: Array[String]): Unit = {
//    Thread.setDefaultUncaughtExceptionHandler((t, e) =>
//      println("Uncaught exception in thread: ")
//    )
    val p = parMap(List.range(1, 2) :+ 0)(1 / _)
//    val p = lazyUnit(???)
    val x = run(Executors.newFixedThreadPool(2))(p)
    println(x)
    println("koniec")
  }
}
