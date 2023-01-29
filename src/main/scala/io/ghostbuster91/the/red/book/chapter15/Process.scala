package io.ghostbuster91.the.red.book.chapter14

import io.ghostbuster91.the.red.book.chapter13.Free._
import io.ghostbuster91.the.red.book.chapter14.Process.Await
import io.ghostbuster91.the.red.book.chapter14.Process.Emit
import io.ghostbuster91.the.red.book.chapter14.Process.Halt
import io.ghostbuster91.the.red.book.nonblocking.NonBlocking
import io.ghostbuster91.the.red.book.chapter13.Free
import java.io.BufferedReader
import java.io.FileReader
import scala.util.Try
import java.io.FileWriter

sealed trait Process[F[_], O] {
  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    case Emit(head, tail) => Emit(head, tail.onHalt(f))
    case Halt(err)        => Process.Try(f(err))
  }

  def onComplete(f: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case Process.End => f
      case err         => f ++ Halt(err)
    }

  def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case Process.End => p
    case err         => Halt(err)
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = {
    this match {
      case Halt(err)        => Halt(err)
      case Emit(head, tail) => Process.Try(f(head)) ++ tail.flatMap(f)
      case Await(req, recv) =>
        Await(req, recv andThen (_ flatMap f))
    }
  }

  def asFinalizer: Process[F, O] = this match {
    case Await(req, recv) =>
      Process.await(req) {
        case Left(Process.Kill) => this.asFinalizer
        case x                  => recv(x)
      }
    case Emit(head, tail) => Emit(head, tail.asFinalizer)
    case Halt(err)        => Halt(err)
  }

  def repeat: Process[F, O] = {
    def go(p: Process[F, O]): Process[F, O] = {
      p match {
        case Emit(head, tail)  => Emit(head, go(tail))
        case Halt(Process.End) => go(this)
        case Halt(err)         => Halt(err)
        case Await(req, recv) =>
          Process.await(req) {
            case Left(err)    => recv(Left(err))
            case Right(value) => go(recv(Right(value)))
          }

      }
    }
    go(this)
  }

  def |>[O2](p2: Process.Process1[O, O2]): Process[F, O2] = {
    p2 match {
      case Process.Halt(e)          => this.kill.onHalt { e2 => Halt(e) ++ Halt(e2) }
      case Process.Emit(head, tail) => Emit(head, this |> tail)
      case Process.Await(req, recv) =>
        this match {
          case Halt(err)          => Halt(err) |> recv(Left(err))
          case Process.Emit(h, t) => t |> Process.Try(recv(Right(h)))
          case Await(req0, recv0) =>
            Process.await(req0)(recv0 andThen (_ |> p2))
        }
    }
  }

  def kill[O2]: Process[F, O2] = this match {
    case Await(req, recv) =>
      recv(Left(Process.Kill)).drain.onHalt {
        case Process.Kill => Halt(Process.End)
        case e            => Halt(e)
      }
    case Halt(e)          => Halt(e)
    case Emit(head, tail) => tail.kill
  }

  def drain[O2]: Process[F, O2] = this match {
    case Halt(err)        => Halt(err)
    case Emit(head, tail) => tail.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))

  }

  def tee[O2, O3](
      p2: Process[F, O2]
  )(t: Process.Tee[O, O2, O3]): Process[F, O3] = {
    t match {
      case Halt(e)    => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h, t) => Emit(h, (this tee p2)(t))
      case Await(side, recv) =>
        side.get match {
          case Left(isO) =>
            this match {
              case Halt(e)    => p2.kill onComplete (Halt(e))
              case Emit(h, t) => (t tee p2)(recv(Right(h)))
              case Await(reqL, recvL) =>
                Process.await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
            }
          case Right(isO2) =>
            p2 match {
              case Halt(e)    => this.kill onComplete (Halt(e))
              case Emit(h, t) => (this tee t)(recv(Right(h)))
              case Await(reqR, recvR) =>
                Process.await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
            }
        }
    }
  }

  def to[O2](sink: Process.Sink[F, O]): Process[F, Unit] = {
    Process.join { (this zipWith sink)((o, f) => f(o)) }
  }

  def through[O2](p2:Process[F,O=>Process[F,O2]]):Process[F,O2]={
    Process.join { (this zipWith p2)((o, f) => f(o)) }
  }

  def zipWith[O2, O3](
      p2: Process[F, O2]
  )(f: (O, O2) => O3): Process[F, O3] = {
    tee(p2)(Process.zipWith(f))
  }
}

object Process {
  case class Await[F[_], A, O](
      req: F[A],
      recv: Either[Throwable, A] => Process[F, O]
  ) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object Kill extends Throwable
  case object End extends Throwable

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }
  def Get[I] = Is[I]().Get

  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
    val L = new f[I] { def get = Left(identity) }
    val R = new f[I2] { def get = Right(identity) }
  }

  def L[I, I2] = T[I, I2]().L
  def R[I, I2] = T[I, I2]().R

  type Process1[I, O] = Process[Is[I]#f, O]
  type Tee[I, I2, O] = Process[T[I, I2]#f, O]
  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] = {
    try (p)
    catch { case e: Throwable => Halt(e) }
  }

  def await[F[_], A, O](
      req: F[A]
  )(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = {
    Await(req, recv)
  }

  def await1[I, O](
      recv: I => Process1[I, O],
      fallback: Process1[I, O] = halt1[I, O]
  ): Process1[I, O] = {
    Await(
      Get[I],
      (e: Either[Throwable, I]) =>
        e match {
          case Left(End) =>
            fallback
          case Left(err) =>
            Halt(err)
          case Right(value) =>
            Try(recv(value))
        }
    )
  }

  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

  def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Emit(h, t1)

  def lift[I, O](f: I => O): Process1[I, O] =
    await1[I, O](i => emit1(f(i))).repeat

  def filter[I](f: I => Boolean): Process1[I, I] = {
    await1[I, I](i => if (f(i)) emit1(i) else halt1).repeat
  }

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = {
      cur match {
        case Await(req, recv) =>
          val next =
            try recv(Right(Free.unsafePerformIO(req)(E)))
            catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
        case Emit(head, tail) => go(tail, acc :+ head)
        case Halt(End)        => acc
        case Halt(err)        => throw err
      }
    }
    try go(src, IndexedSeq())
    finally E.shutdown()
  }

  val p1: Process[IO, String] = {
    await(IO(new BufferedReader(new FileReader("lines.txt")))) {
      case Right(buffer) =>
        lazy val next: Process[IO, String] = await(IO(buffer.readLine)) {
          case Left(e) => await(IO(buffer.close()))(_ => Halt(e))
          case Right(line) =>
            if (line eq null) Halt(End)
            else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }
  }

  def resource[R, O](
      acquire: IO[R]
  )(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] = {
    await(acquire) {
      case Left(value) =>
        Halt(value)
      case Right(value) =>
        use(value).onComplete(release(value))
    }
  }

  def eval[F[_], A](fa: F[A]): Process[F, A] = {
    await(fa) {
      case Right(value) =>
        Emit(value, Halt(End))
      case Left(value) => Halt(value)
    }
  }

  def eval_[F[_], A, B](fa: F[A]): Process[F, B] = {
    await(fa) {
      case Right(value) => Halt(End)
      case Left(value)  => Halt(value)
    }
  }

  def lines(filename: String): Process[IO, String] = {
    resource { IO(scala.io.Source.fromFile(filename)) } { src =>
      lazy val iter = src.getLines
      def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    } { src => eval_(IO(src.close)) }
  }

  def fileW(file: String, append: Boolean): Sink[IO, String] = {
    resource[FileWriter, String => Process[IO, Unit]] {
      IO { new FileWriter(file, append) }
    } { w => constant { (s: String) => eval[IO, Unit](IO(w.write(s))) } } { w =>
      eval_(IO(w.close))
    }
  }

  def constant[A](a: A): Process[IO, A] = {
    eval[IO, A](IO(a)).repeat
  }

  def haltT[I, I2, O]: Tee[I, I2, O] = {
    Halt[T[I, I2]#f, O](End)
  }

  def awaitL[I, I2, O](
      recv: I => Tee[I, I2, O],
      fallback: => Tee[I, I2, O] = haltT[I, I2, O]
  ): Tee[I, I2, O] = {
    await[T[I, I2]#f, I, O](L) {
      case Left(End)   => fallback
      case Left(value) => Halt(value)
      case Right(a)    => Try(recv(a))
    }
  }

  def awaitR[I, I2, O](
      recv: I2 => Tee[I, I2, O],
      fallback: => Tee[I, I2, O] = haltT[I, I2, O]
  ): Tee[I, I2, O] = {
    await[T[I, I2]#f, I2, O](R) {
      case Left(End)   => fallback
      case Left(value) => Halt(value)
      case Right(a)    => Try(recv(a))
    }
  }

  def emitT[I, I2, O](
      h: O,
      tl: Tee[I, I2, O] = haltT[I, I2, O]
  ): Tee[I, I2, O] = {
    Emit(h, tl)
  }

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = {
    awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)).repeat))
  }

  def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

  def join[F[_],O](p:Process[F, Process[F,O]]):Process[F,O]={
    p.flatMap(p2=> p2)
  }

}
