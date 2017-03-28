package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

/**
  * Created by Gnob on 2017. 3. 26..
  */
object AbsPar {
  def oldSum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b) => a + b)

  def dacSum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      dacSum(l) + dacSum(r)
    }

  def unit[A](a: => A): Par[A] = ???

  // as container
  def get[A](a: Par[A]): A = ???

  def blockingSum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = AbsPar.unit(blockingSum(l))
      val sumR: Par[Int] = AbsPar.unit(blockingSum(r))
      AbsPar.get(sumL) + AbsPar.get(sumR)
    }

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = ???

  def obscureSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      AbsPar.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      AbsPar.map2(obscureSum(l), obscureSum(r))(_ + _)
    }

  def fork[A](a: => Par[A]): Par[A] = ???

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      AbsPar.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      AbsPar.map2(AbsPar.fork(sum(l)), AbsPar.fork(sum(r)))(_ + _)
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // as predicate
  def run[A](a: Par[A]): A = ???

  // run knows parallelism
  def run[A](s: ExecutorService)(a: Par[A]): A = ???

  type Par[A] = ExecutorService => Future[A]

  // return Future for user to control result
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

}

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  private case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined
    def get(): C = compute(Long.MaxValue, TimeUnit.NANOSECONDS)
    def get(timeout: Long, unit: TimeUnit): C = compute(timeout, unit)
    def isCancelled: Boolean = a.isCancelled || b.isCancelled
    def cancel(mayInterruptIfRunning: Boolean): Boolean =
      a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

    def compute(timeout: Long, unit: TimeUnit): C = {
      val timeoutInNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
      cache match {
        case Some(x) => x
        case None =>
          val start = System.nanoTime()
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val end = System.nanoTime()
          val aTime = end - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }
  }

  def map2WithTimeout[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      Map2Future(af, bf, f)
    }
}
