package week4

trait Option[+A] { self =>
  def map[B](f: A => B): Option[B] = flatMap(f andThen Some.apply _)

  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case Some(value) => f(value)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some.apply _).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}

case object None extends Option[Nothing]
case class Some[T](value: T) extends Option[T]

object Ch4 {
  private[this] def calcMean(xs: Seq[Double]): Option[Double] = {
    if (xs.nonEmpty) {
      Some(xs.sum / xs.length)
    } else {
      None
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = calcMean(xs)

    m.map(m => xs.map(x => Math.pow(x - m, 2))).flatMap(calcMean)
  }

  def map2[A, B, C](o1: Option[A], o2: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- o1
      y <- o2
    } yield f(x, y)
  }
}
