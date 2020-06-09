package io.ghostbuster91.the.red.book

object Fib {

  def fib(n: Int): Int = {
    def go(prevprev: Int, prev: Int, nth: Int): Int = {
      if (nth == 1) prevprev
      else
        go(prevprev = prev, prev = prevprev + prev, nth - 1)
    }
    go(0, 1, n)
  }

  def fib2(n: Int): Int = {
    if (n == 1) 0 else if (n == 2) 1 else fib2(n - 2) + fib2(n - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(acc: Boolean, n: Int): Boolean = {
      if (n == as.length - 1) acc
      else loop(acc && ordered(as(n), as(n + 1)), n + 1)
    }

    loop(acc = true, 0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 4, 3, 4), { (x: Int, y: Int) =>
      x <= y
    }))
  }
}
