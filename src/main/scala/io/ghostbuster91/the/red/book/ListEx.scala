package io.ghostbuster91.the.red.book

sealed trait List[+A] { // `List` data type, parameterized on a type, `A` {
  def max[N >: A](implicit ev: Numeric[N]): N
}
case object Nil // A `List` data constructor representing the empty list
    extends List[
      Nothing
    ] {
  override def max[N >: Nothing](implicit ev: Numeric[N]): N =
    throw new RuntimeException("max on empty")
}

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  def max[N >: A](implicit ev: Numeric[N]) = {
    ListEx.foldLeft(tail, head)((b, a) => ev.max(a, b))
  }
}

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil

  def fill[A](n: Int)(item: => A): List[A] = {
    io.ghostbuster91.the.red.book.strictness.Stream
      .unfoldConstant(item)
      .take(n)
      .toList
  }
}

object ListEx {

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case o                   => o
    }
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
    l match {
      case Cons(head, next) if p(head) => dropWhile(next, p)
      case o                           => o
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil              => z
      case Cons(head, next) => f(head, foldRight(next, z)(f))
    }
  }

  @annotation.tailrec
  def foldLeftFromDocs[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeftFromDocs(t, f(z, h))(f)
  }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))
  }
  def foldRightUsingLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((x, y) => f(y, x))
  }

  def append[A](as: List[A], bs: List[A]): List[A] = {
    as match {
      case Nil              => bs
      case Cons(head, tail) => Cons(head, append(tail, bs))
    }
  }

  def appendFold[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)((acc, h) => Cons(acc, h))
  }

  def concatMany[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil: List[A])(append)
  }

  def addOne(as: List[Int]): List[Int] = {
    as match {
      case Cons(head, next) => Cons(head + 1, addOne(next))
      case _                => as
    }
  }

  def addOneFold(as: List[Int]): List[Int] = {
    foldRight(as, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  def doubleToString(as: List[Double]): List[String] = {
    foldRight(as, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(head, next) if (f(head)) =>
        Cons(head, filter(next)(f))
      case Cons(head, next) => filter(next)(f)
      case _                => as
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => appendFold(f(x), y))
  }

  def filter3[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def combineInts(as: List[Int], bs: List[Int]): List[Int] = {
    zipWith(as, bs)(_ + _)
  }

  def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] = {
    @scala.annotation.tailrec
    def go(acc: List[B], t1: List[A], t2: List[A]): List[B] = {
      (t1, t2) match {
        case (Cons(h1, tt1), Cons(h2, tt2)) =>
          go(Cons(f(h1, h2), acc), tt1, tt2)
        case (Nil, Nil) => acc
      }
    }
    reverse(go(Nil: List[B], as, bs))
  }
  @scala.annotation.tailrec
  def hasSubseq[A](as: List[A], bs: List[A]): Boolean =
    (as, bs) match {
      case (_, Nil)                                                    => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 && startsWith2(t, t2) => true
      case (Cons(_, t), _)                                             => hasSubseq(t, bs)
      case (Nil, _)                                                    => false
    }

  @scala.annotation.tailrec
  def startsWith2[A](as: List[A], bs: List[A]): Boolean = {
    (as, bs) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith2(t1, t2)
      case _                                        => false
    }
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                              => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _                                     => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t)                => hasSubsequence(t, sub)
  }

  def toString[A](l: List[A]): String = {
    l match {
      case Nil => "[]"
      case Cons(head, tail) =>
        "[" + foldRight(tail, s"$head")((a, b) => b + s", $a") + "]"
    }

  }

  def main(args: Array[String]): Unit = {
    println(reverse(List(1, 2, 3, 4, 5, 6)))
    println(foldLeft(List(1, 2, 3, 4, 5, 6), 0)((x, y) => x - y))
    // (0 - 1) - 2) - 3) - 4) - 5)

    println("foldRight")
    println(foldRight(List(1, 2, 3, 4, 5, 6), 0)((x, y) => x - y))
    /// Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))
    // f(1, f(2, f(3, f(4, f(5, 0)
    // 1 - (2 - (3 - (4 - (5 - 0)
    // 3
    println("foldRightUsingLeft")
    println(foldRightUsingLeft(List(1, 2, 3, 4, 5, 6), 0)((x, y) => x - y))
    // (1 - 0) - 2) - 3) - 4) - 5)
    println("append")
    println(append(List(1, 2), List(3)))
    println(appendFold(List(1, 2), List(3)))
    println("append -end")
    println(concatMany(List(List(1), List(2, 3, 4), List(4, 5, 6))))
    println(addOne(List(1, 2, 3, 4)))
    println(addOneFold(List(1, 2, 3, 4)))
    println(doubleToString(List(1, 2, 3, 4)))
    println(map(List(1, 2, 3, 4))(_ + 1))
    println("filter")
    println(filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
    println(filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
    println("flatMap")
    println(flatMap(List(1, 2, 3, 4, 5, 6))(i => List(i, i)))
    println(filter3(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))
    println(combineInts(List(1, 2, 3), List(4, 5, 6)))

    println(hasSubseq(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubseq(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubseq(List(1, 2, 3, 4), List(3, 4)))
    println(hasSubseq(List(1, 2, 3, 4), List(1, 4)))

    println("docs")
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(3, 4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 4)))

    println("maj")
    println(ListEx.foldRight(List(1, 2, 3), 0) { (a, b) =>
      println(s"computing $a + $b"); a + b
    })
  }
}
