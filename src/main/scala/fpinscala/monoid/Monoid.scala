package fpinscala.monoid

/**
  * Monoid
  *
  * @author SangBong Lee
  * @version 2.0.0
  * @since 2017. 05. 08.
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  // Practice 10.1
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  // Practice 10.1
  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  // Practice 10.1
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = true
  }

  // Practice 10.1
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = false
  }

  // Practice 10.2
  def myOptionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1.isDefined) a1 else a2
    override def zero: Option[A] = None
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]) = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1 compose a2
    override def zero = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Practice 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Practice 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f.curried(a))(z)

  // Practice 10.6
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // Practice 10.7
  def myFoldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val len = v.length
    if (len == 1) return f(v.head)
    val (left, right) = v.splitAt(len / 2)
    m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

  // Practice 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val len = v.length
    if (len == 0) m.zero
    else if (len == 1) f(v.head)
    else {
      val (l, r) = v.splitAt(len / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // Practice 10.9
  def sortMonoid = new Monoid[IndexedSeq[Int]] {
    override def op(a1: IndexedSeq[Int], a2: IndexedSeq[Int]): IndexedSeq[Int] = ???
    override def zero: IndexedSeq[Int] = ???
  }
}
