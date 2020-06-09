package io.ghostbuster91.the.red.book

object TreeEx {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = {
//    tree match {
//      case Leaf(value)         => 1
//      case Branch(left, right) => size(left) + size(right) + 1
//    }
    fold[A, Int](tree)(_ => 1, _ + _ + 1)
  }

  def maximum(tree: Tree[Int]): Int = {
//    tree match {
//      case Leaf(value)         => value
//      case Branch(left, right) => maximum(left).max(maximum(right))
//    }
    fold[Int, Int](tree)(identity, _ max _)
  }

  def depth[A](tree: Tree[A]): Int = {
//    tree match {
//      case Leaf(value)         => 1
//      case Branch(left, right) => depth(left).max(depth(right)) + 1
//    }
    fold[A, Int](tree)(_ => 1, (x, y) => (x max y) + 1)
  }

  def fold[A, B](tree: Tree[A])(f: A => B, f2: (B, B) => B): B = {
    tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) =>
        f2(fold(left)(f, f2), fold(right)(f, f2))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
//    tree match {
//      case Leaf(value)         => Leaf(f(value))
//      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
//    }
    fold[A, Tree[B]](tree)(x => Leaf(f(x)), (x, y) => Branch(x, y))
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(
      Leaf(1),
      Branch(
        Leaf(10),
        Branch(
          Branch(
            Branch(Leaf(11), Branch(Leaf(4), Leaf(2))),
            Branch(Leaf(12), Leaf(2))
          ),
          Branch(Leaf(1), Leaf(2))
        )
      )
    )
    println(size(tree))
    println(maximum(tree))
    println(depth(tree))
    println(map(tree)(x => s"$x $x"))
  }

}
