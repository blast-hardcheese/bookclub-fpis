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

object Option {
  def apply[T](x: T): Option[T] = Some(x).filter(_ != null)
  def empty[T]: Option[T] = None
}

case object None extends Option[Nothing]
case class Some[T](value: T) extends Option[T]

sealed trait Either[+E, +A] { self =>
  def map[B](f: A => B): Either[E, B] = flatMap(f andThen Right.apply _)
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
    case Right(value) => f(value)
    case Left(err) => Left(err)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(o => b.map(i => f(o, i)))
  }

  def isRight: Boolean
  def isLeft: Boolean = !isRight
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def isRight = false
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  def isRight = true
}

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

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Option(List.empty[A])) { (acc: Option[List[A]], x: Option[A]) =>
      acc flatMap { xs =>
        x.map(xs :+ _)
      }
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Option(List.empty[B])) { (acc: Option[List[B]], x: A) =>
      acc flatMap { xs =>
        f(x) map { xs :+ _ }
      }
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}
