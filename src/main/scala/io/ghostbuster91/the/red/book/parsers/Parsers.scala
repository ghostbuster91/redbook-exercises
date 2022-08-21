package io.ghostbuster91.the.red.book.parsers

import io.ghostbuster91.the.red.book.parsers.A.Parser

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  def attempt[A](p: Parser[A]): Parser[A]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def fail[A]: Parser[Nothing]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def succeed[A](a: A): Parser[A]
  def manyRec[A](p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(
      attempt(map2(p, many(p)) { case (a, b) =>
        a :: b
      }),
      succeed(Nil)
    )

  def opt[A](p: Parser[A]): Parser[Option[A]] = {
    or(map(attempt(p))(Some(_)), succeed(None))
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p)) {
    case (a, b) => a :: b
  }

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    flatMap(pa)(a => map(pb)(b => (a, b)))

  def sequence1[A](pa: Parser[A], ps: Parser[A]*): Parser[List[A]] = {
    ps.foldLeft(map(pa)(p => List(p))) { (acc, item) =>
      map2(acc, item) { case (a, b) => a :+ b }
    }
  }

  def choice1[A](c1: Parser[A], cs: Parser[A]*): Parser[A] = {
    cs.foldLeft(c1)((a, b) => or(a, b))
  }

  def void[A](parser: Parser[A]): Parser[Unit] = map(parser)(_ => ())

  def merge[A](pa: Parser[A], pb: Parser[Unit]): Parser[A] =
    map2(pa, pb)((a, _) => a)
  def merge2[A](pb: Parser[Unit], pa: Parser[A]): Parser[A] =
    map2(pb, pa)((_, a) => a)

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] = flatMap(pa)(a => map(pb)(b => f(a, b)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n > 1)
    map2(p, listOfN(n - 1, p)) { case (a, b) => a :: b }
  else map(p)(a => List(a))

  def run[A](p: Parser[A])(input: String): Result[A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.manyRec(p)
    def opt: Parser[Option[A]] = self.opt(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](b: Parser[B]): Parser[(A, B)] = self.product(p, b)
    def void: Parser[Unit] = self.void(p)
    def attempt: Parser[A] = self.attempt(p)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(s: String): Parser[A] = self.scope(s)(p)
  }

  val numA: Parser[Int] = char('c').many.map(_.size)

}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)
  def latest: Option[(Location, String)] = stack.lastOption
  def latestLoc: Option[Location] = latest.map(_._1)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)

}

sealed trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = {
    this match {
      case failure: Failure[A] => failure.copy(get = f(failure.get))
      case success: Success[A] => success
    }
  }
  def uncomit: Result[A] = this match {
    case s @ Success(get, charsConsumed) => s
    case f: Failure[A]                   => f.copy(isCommitted = false)
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(get, c) => Failure(get, c || isCommitted)
    case other           => other
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(get, charsConsumed) => Success(get, charsConsumed + n)
    case Failure(get, isCommitted)   => Failure(get, isCommitted)
  }
}
case class Success[A](get: A, charsConsumed: Int) extends Result[A]
case class Failure[A](get: ParseError, isCommitted: Boolean)
    extends Result[Nothing]
//class Parser[+A]

object Parser extends Parsers[Parser] {
  override def regex(r: Regex): Parser[String] = (state: Location) =>
    r.findPrefixMatchOf(state.input.drop(state.offset)) match {
      case Some(value) => Success(value.matched, value.matched.length)
      case None =>
        Failure(state.toError(s"Regex failure: $r"), isCommitted = true)
    }

  override def slice[A](p: Parser[A]): Parser[String] = { (state: Location) =>
    p(state) match {
      case Success(_, charsConsumed) =>
        Success(state.input.slice(state.offset, charsConsumed), charsConsumed)
      case failure: Failure[A] => failure
    }
  }

  override def flatMap[A, B](
      p: Parser[A]
  )(f: A => Parser[B]): Parser[B] = { (state: Location) =>
    p(state) match {
      case failure: Failure[A] => failure
      case Success(get, charsConsumed) =>
        f(get)(state.advanceBy(charsConsumed))
          .addCommit(charsConsumed != 0)
          .advanceSuccess(charsConsumed)
    }
  }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = {
    (state: Location) =>
      s1(state) match {
        case Failure(_, false) => s2(state)
        case other             => other
      }
  }

  override def succeed[A](a: A): Parser[A] = { (_: Location) =>
    Success(a, 0)
  }

  override implicit def string(s: String): Parser[String] =
    (state: Location) =>
      if (state.input.startsWith(s, state.offset)) {
        Success(s, s.length)
      } else {
        Failure(
          state.toError(
            s"Expected ${s}, but got '${state.input
              .slice(state.offset, state.offset + s.length)}'  "
          ),
          isCommitted = true
        )
      }

  override def run[A](p: Parser[A])(
      input: String
  ): Result[A] = p.apply(Location(input))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    (state: Location) =>
      p(state).mapError(_.label(msg))
  }

  override def attempt[A](p: Parser[A]): Parser[A] = { (state: Location) =>
    p(state).uncomit
  }

  override def fail[A]: Parser[Nothing] = (state: Location) =>
    Failure(state.toError("User defined failure"), isCommitted = true)

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
    (state: Location) => p(state).mapError(_.push(state, msg))
  }

  override def manyRec[A](p: Parser[A]): Parser[List[A]] = {
    (state: Location) =>
      @tailrec
      def loop(acc: Success[List[A]]): Result[List[A]] = {
        p.attempt(state.advanceBy(acc.charsConsumed)) match {
          case Success(get, charsConsumed) =>
            loop(Success(acc.get :+ get, acc.charsConsumed + charsConsumed))
          case Failure(get, false)    => acc
          case f @ Failure(get, true) => f
        }

      }
      loop(Success(Nil, 0))
  }
}

object A {
  type Parser[+A] = Location => Result[A]
}

object Test {

  def jsonParser(P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val whitespace = regex("\\s".r).many.void
    def commaSep1[A](p: Parser[A]): Parser[List[A]] = map2(
      p.opt,
      merge2(merge(char(',').void, whitespace), p).many
    ) { case (a, b) =>
      a.toList ++ b
    }
    def surround[A](
        p1: Parser[Unit],
        p2: Parser[A],
        p3: Parser[Unit]
    ): Parser[A] = merge2(p1.void, merge(p2, p3.void))
    lazy val jNumber: Parser[JSON] =
      regex("\\d".r).map(d => JSON.JNumber(d.toDouble)).scope("jNumber")
    lazy val jNull: Parser[JSON] =
      string("null").map(_ => JSON.JNull).scope("jNull")
    lazy val jBool: Parser[JSON] = or(
      string("false").attempt.map(_ => JSON.JBool(false)),
      string("true").map(_ => JSON.JBool(true))
    ).scope("jBool")
    lazy val jString: Parser[JSON.JString] =
      regex("\"(.*?)\"".r).map(JSON.JString).scope("jString")
    lazy val jJson: Parser[JSON] =
      or(
        jNull.attempt,
        or(
          jBool.attempt,
          or(jString.attempt, or(jNumber.attempt, or(jArray, jObject)))
        )
      ).scope("json")
    lazy val jArray: Parser[JSON] = surround(
      char('[').attempt.void,
      surround(whitespace, commaSep1(jJson), whitespace),
      char(']').void
    )
      .map(arr => JSON.JArray(arr.toIndexedSeq))
      .scope("jArray")
    lazy val jField: Parser[(String, JSON)] =
      product(
        merge(jString, whitespace.void),
        merge2(merge(char(':').void, whitespace), jJson)
      ).map { case (string, product) =>
        string.get -> product
      }.scope("jField")
    lazy val jObject: Parser[JSON] = surround(
      char('{').void,
      surround(whitespace, commaSep1(jField), whitespace),
      char('}').void
    )
      .map(fields => JSON.JObject(fields.toMap))
      .scope("jObject")
    jJson
  }
  def main(args: Array[String]): Unit = {
    import Parser._
    println(Parser.run(jsonParser(Parser))("[1, 2]"))
    println(Parser.run(jsonParser(Parser))("[1, \"2\"]"))
    println(Parser.run(jsonParser(Parser))("{}"))
    println(Parser.run(jsonParser(Parser))("null"))
    println(Parser.run(jsonParser(Parser))("false"))
    println(Parser.run(jsonParser(Parser))("true"))
    println(Parser.run(jsonParser(Parser))("1"))
    println(Parser.run(jsonParser(Parser))("{\"a\": 1}"))

    println(
      Parser.run(jsonParser(Parser))(
        "{\"f1\": [1, \"2\"], \"f1\": [true, {\"f1\": [null]}]}"
      )
    )

    Parser.run(jsonParser(Parser))(
      List.fill(10000)(1).mkString("[", ",", "]")
    ) match {
      case Success(get, charsConsumed) => println("Success 10k")
      case Failure(get, isCommitted)   => println(get)
    }
  }
}
