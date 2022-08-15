package io.ghostbuster91.the.red.book.parsers

import scala.util.matching.Regex

trait Parser[T]

trait ParseError

trait Parsers[ParseError, Parser[+_]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def regex(r: Regex): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(
      map2(p, many(p)) { case (a, b) =>
        a :: b
      },
      succeed(Nil)
    )
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p)) {
    case (a, b) => a :: b
  }

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    flatMap(pa)(a => map(pb)(b => (a, b)))

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] = flatMap(pa)(a => map(pb)(b => f(a, b)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n > 1)
    map2(p, listOfN(n - 1, p)) { case (a, b) => a :: b }
  else map(p)(a => List(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](b: Parser[B]): Parser[(A, B)] = self.product(p, b)
  }

  val numA: Parser[Int] = char('c').many.map(_.size)

}
