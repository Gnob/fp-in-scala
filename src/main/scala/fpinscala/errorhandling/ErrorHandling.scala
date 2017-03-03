package fpinscala.errorhandling

/**
  * TryCatch
  *
  * @author Gnob
  * @since 2017. 03. 02.
  */
object ErrorHandling {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("Fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("Fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  /*
  완전 함수이지만 유연하지 못하다.
  failFnSpec.scala 참조
   */
  def mean1(xs: Seq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean2(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
