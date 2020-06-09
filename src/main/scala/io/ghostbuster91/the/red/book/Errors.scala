package io.ghostbuster91.the.red.book

import scala.collection.immutable
import scala.collection.immutable.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(get) => Some(f(get))
      case None      => None
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(get) => get
      case None      => default
    }
  }
  def orElse[B >: A](or: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(or)
  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap { a =>
      if (f(a)) Some(a) else None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def none[A]: Option[A] = None
  def some[A](v: A): Option[A] = Some(v)
}

object Errors {
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def map2[A, B, C](a: Option[A], b: Option[B], f: (A, B) => C): Option[C] = {
    a.flatMap(av => b.map(bv => f(av, bv)))
  }

  def map3[A, B, C, D](a: Option[A],
                       b: Option[B],
                       c: Option[C],
                       f: (A, B, C) => D): Option[D] = {
    c.flatMap(cv => map2(a, b, (av: A, bv: B) => f(av, bv, cv)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse[Option[A], A](a)(identity)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case ::(head, next) =>
        f(head) flatMap (hv => traverse(next)(f).map(hv :: _))
      case immutable.Nil => Some(List.empty)
    }
  }
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(value)  => Left(value)
      case Right(value) => Right(f(value))
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_)      => b
      case Right(value) => Right(value)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(av => b.map(bv => f(av, bv)))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }

  def traverse[E, A, B](
    as: List[A]
  )(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case ::(head, next) => f(head).flatMap(hv => traverse(as)(f).map(hv :: _))
      case immutable.Nil  => Right(List.empty[B])
    }
  }
}

object Test {

  def mkPerson(age: Int, name: String): Partial[String, Person] = {
    for {
      ageE <- mkAge(age)
      nameE <- mkName(name)
    } yield Person(ageE, nameE)
  }

  def mkName(name: String): Partial[String, String] = {
    if (name.isEmpty) {
      Errors(Seq("name cannot be empty"))
    } else {
      Success(name)
    }
  }

  def mkAge(age: Int): Partial[String, Int] = {
    if (age <= 0) {
      Errors(Seq("Age has to be greater than zero"))
    } else {
      Success(age)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Errors.sequence(List(Some(1), Some(2), Some(3))))
    println(Errors.sequence(List(Some(1), Some(2), None)))
    println(Errors.sequence(List(Some(1), None, Some(3))))
    println(Errors.sequence(List(None, Some(2), Some(3))))

    println(mkPerson(1, "asd"))
    println(mkPerson(-1, "asd"))
    println(mkPerson(-1, ""))
    println(mkPerson(1, ""))
  }
}

case class Person(age: Int, name: String)

sealed trait Partial[+A, +B] {
  def map[C](f: B => C): Partial[A, C] = {
    this match {
      case Errors(value)  => Errors(value)
      case Success(value) => Success(f(value))
    }
  }
  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = {
    this match {
      case Errors(get)  => Errors(get)
      case Success(get) => f(get)
    }
  }

  def orElse[AA >: A, BB >: B](b: => Partial[AA, BB]): Partial[AA, BB] = {
    this match {
      case Errors(get) =>
        b match {
          case Errors(get2)  => Errors(get ++ get2)
          case Success(get2) => Success(get2)
        }
      case Success(get) => Success(get)
    }
  }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = {
    flatMap(av => b.map(bv => f(av, bv)))
  }
}
case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
case class Success[+B](get: B) extends Partial[Nothing, B]
