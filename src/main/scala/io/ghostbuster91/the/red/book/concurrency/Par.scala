package io.ghostbuster91.the.red.book.concurrency

import io.ghostbuster91.the.red.book.concurrency.Par.{Par, parMap, parReduce, unit}

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

private case class UnitFuture[A](get: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def run[A](a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { es =>
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)
  def lazyUnit[A](a: => A): Par[A] = Par.fork(Par.unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  def map[A,B](a: Par[A])(f: A=>B): Par[B] =map2(a,unit(()))((a,_)=>f(a))
  def sequence[A](ps: List[Par[A]]): Par[List[A]]  ={
    ps match {
      case ::(head, next) => map2(head,fork(sequence(next))){(a,b)=> a :: b  }
      case Nil =>unit(List.empty)
    }
  }

  def parMap[A,B](l: List[A])(f: A=> B) : Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    sequence(fbs)
  }
  def parFilter[A](l: List[A])(f: A=> Boolean) : Par[List[A]] = fork {
    val l2 = l.map(e => map(asyncF[A,Boolean](f)(e)){ r => if(r) List(e) else List.empty })
    map(sequence(l2))(_.flatten)
  }

  def parReduce[A](as: IndexedSeq[Par[A]])(empty: => A, op: (A,A)=>A) : Par[A] = {
    if(as.size <= 1) {
      unit(empty)
    }else{
      val (l,r) = as.splitAt(as.length / 2)
      map2(fork(parReduce(l)(empty,op)),parReduce(r)(empty,op))(op)
    }
  }

  def parSeq2[A,B](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if(as.size <= 1) {
      unit(Vector.empty)
    }else{
      val (l,r) = as.splitAt(as.length / 2)
      map2(fork(parSeq2(l)),parSeq2(r))(_ ++ _)
    }
  }

  def count(as : List[String]): Par[Int] = fork {
    val ps = as.map(asyncF(s=>s.count(_ == ' ')))
    parReduce(ps.toIndexedSeq)(0,_+_)
  }
}


object Main {


  def sum_1(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum_1(l) + sum_1(r)
    }
  }

  def sum_2(ints: IndexedSeq[Int]): Par[Int] = {
    parReduce(ints.map(unit))(0, _+_)
  }
}
