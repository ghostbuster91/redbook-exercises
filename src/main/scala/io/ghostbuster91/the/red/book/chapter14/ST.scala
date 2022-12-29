package io.ghostbuster91.the.red.book.chapter14

import io.ghostbuster91.the.red.book.chapter12.Applicative
import io.ghostbuster91.the.red.book.chapter12.Traverse

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = {
    new ST[S, B] {
      protected def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] =
    new ST[S, B] {
      protected def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      override protected def run(s: S): (A, S) = {
        (memo, s)
      }
    }
  }

  def runST[A](st: RunnableST[A]): A = {
    st.apply[Unit].run(())._1
  }

  implicit def app[S] = {
    new Applicative[({ type f[x] = ST[S, x] })#f] {

      override def unit[A](a: => A): ST[S, A] = ST(a)

      def map2[A, B, C](fa: ST[S, A], fb: ST[S, B])(
          f: (A, B) => C
      ): ST[S, C] = {
        fa.flatMap(a => fb.map(b => f(a, b)))
      }
    }
  }

}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
  def modify(f: A => A): ST[S, Unit] = {
    for {
      x <- read
      _ <- write(f(x))
    } yield ()
  }
}
object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    protected var cell: A = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    protected def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = {
    type STS[A] = ST[S, A]
    Traverse.listTraverse
      .sequence(xs.map { case (k, v) => write(k, v) }.toList)
      .map(_ => ())
  }

  def swap(i: Int, j: Int): ST[S, Unit] = {
    for {
      a <- read(i)
      b <- read(j)
      _ <- write(i, b)
      _ <- write(j, a)
    } yield ()
  }

}

object STArray {

  implicit class STArrayInt[S](stArray: STArray[S, Int]) {
    def partition(n: Int, r: Int, pivot: Int): ST[S, Int] = {
      for {
        pivotVal <- stArray.read(pivot)
        _ <- stArray.swap(pivot, r)
        j <- STRef(n)
        _ <- Traverse.listTraverse.sequence((n until r).toList.map { i =>
          stArray.read(i).flatMap { iv =>
            if (iv < pivotVal) {
              j.read.flatMap(stArray.swap(i, _)).flatMap(_ => j.modify(_ + 1))
            } else {
              ST(())
            }
          }
        })
        jv <- j.read
        _ <- stArray.swap(jv, r)
      } yield jv
    }

    def qs(n: Int, r: Int): ST[S, Unit] = {
      if (n < r) {
        for {
          pi <- partition(n, r, n + (r - n) / 2)
          _ <- stArray.freeze.map(println)
          _ <- ST(println(pi))
          _ <- qs(n, pi - 1)
          _ <- qs(pi + 1, r)

        } yield ()
      } else ST(())
    }

  }

  def apply[S, A: Manifest](s: Int, a: A): ST[S, STArray[S, A]] = ST(
    new STArray[S, A] {
      val value: Array[A] = Array.fill(s)(a)
    }
  )
  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(
    new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    }
  )
}

object Playground {
  val e1: ST[Nothing, (Int, Int)] = for {
    r1 <- STRef[Nothing, Int](1)
    r2 <- STRef[Nothing, Int](2)
    x <- r1.read
    y <- r2.read
    _ <- r1.write(y + 1)
    _ <- r2.write(x + 1)
    a <- r1.read
    b <- r2.read
  } yield (a, b)

  val p: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }
  def quicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- arr.qs(0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
  }

  def main(args: Array[String]): Unit = {
    val a = quicksort(List(1, 2, 3, 0, 4, 8, 5))
    println(a)
  }
}
