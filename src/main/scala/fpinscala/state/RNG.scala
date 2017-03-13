package fpinscala.state

/**
  * Created by Gnob on 2017. 3. 12..
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def wrongRandomPair(rng: RNG): (Int, Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1,i2), rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt

    if (n == Int.MinValue)
      (0, nextRNG)
    else
      (math.abs(n), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, nextRNG) = double(rng2)

    ((n, d), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, nextRNG) = rng2.nextInt

    ((d, n), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (dd, rng3) = double(rng2)
    val (ddd, nextRNG) = double(rng3)

    ((d, dd, ddd), nextRNG)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (ints, rngs) = Stream.iterate(rng.nextInt)(x => x._2.nextInt).take(count).unzip

    (ints.toList, rngs.last)
  }

//  def int: Rand[Int] = _.nextInt
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(x => x - x % 2)

  def doubleGracefully: Rand[Double] =
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, nextRNG) = rb(rng2)
      (f(a,b), nextRNG)
    }

  def both[A,B](ra: Rand[A], rb:Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, doubleGracefully)

  def randDoubleInt: Rand[(Double, Int)] =
    both(doubleGracefully, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def go(l: List[Rand[A]], rng: RNG): (List[A], RNG) = l match {
      case Nil => (Nil, rng)
      case h :: t =>
        val (nh, nextRNG) = h(rng)
        val (nt, lastRNG) = go(t, nextRNG)
        (nh :: nt, lastRNG)
    }

    rng => { go(fs, rng) }
  }

  // TODO: foldRight나 foldLeft로 구현해보기
  def sequenceByFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = ???


}
