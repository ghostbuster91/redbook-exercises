package io.ghostbuster91

import zio.ZIO

object ZioTest {
  def main(args: Array[String]): Unit = {
    val a = for {
      b <- ZIO.fromEither(
        Left(FirstError()).asInstanceOf[Either[FirstError, String]]
      )
      c <- ZIO.fromEither(
        Left(SecondError()).asInstanceOf[Either[SecondError, String]]
      )
    } yield (b, c)

    a
  }
}

case class FirstError()
case class SecondError()
