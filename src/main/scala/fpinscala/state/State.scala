package fpinscala.state

/**
  * Created by Gnob on 2017. 3. 16..
  */
case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] =
    State(s => {
      val (a, ns) = run(s)
      (f(a), ns)
    })

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => sb.flatMap(b => State.unit(f(a,b))))

  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(s => {
      val (a, ns) = run(s)
      g(a).run(ns)
    })
}

object State {
//  type State[S,+A] = S => (A,S)
//  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State((s: S) => (a,s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    State(s => {
      fs.foldLeft((Nil: List[A], s))((x, s) => {
        val (h, nextRNG) = s.run(x._2)
        (x._1 :+ h, nextRNG)
      })
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insertCoin(): ((Int,Int), Machine) = {
    var isLock = locked
    if (locked && candies > 0)
      isLock = false
    ((coins, candies), Machine(isLock, candies, coins + 1))
  }

  def turnHandle: ((Int,Int), Machine) = {
    if (candies > 0 && !locked)
      ((coins, candies - 1), Machine(true, candies - 1, coins))
    else
      ((coins, candies), Machine(false, candies, coins))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  def toState: State[Machine, (Int, Int)] = State(_ => ((coins, candies), this))
}

object Machine {

}
