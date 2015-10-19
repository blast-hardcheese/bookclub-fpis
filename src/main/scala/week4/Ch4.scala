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

case class Person(name: Name, age: Age)
case class Name(val value: String)
case class Age(val value: Int)

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

  def sequence3[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse2(es)(identity)
  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft[Either[E, List[B]]](Right(List.empty[B])) { (acc, x) =>
      acc flatMap { xs =>
        f(x).map(xs :+ _)
      }
    }
  }

  implicit class RichEither[L, R](e: Either[L, R]) {
    def toAccumulator[R2](acc: R => R2): Either[List[L], R2] = e match {
      case Left(value) => Left(List(value))
      case Right(value) => Right(acc(value))
    }
  }

  implicit class RichAccumulator[L, R, R2](e: Either[List[L], R => R2]) {
    def chain(e2: Either[L, R]): Either[List[L], R2] = (e, e2) match {
      case (Left(l1), Left(l2)) => Left(l1 :+ l2)
      case (l1@Left(_), _) => l1
      case (_, Left(l2)) => Left(List(l2))
      case (Right(r1), Right(r2)) => Right(r1(r2))
    }
  }

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  // Given example. How can we rewrite this to get many errors out?
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def mkPerson2(name: String, age: Int): Either[List[String], Person] = {
    mkName(name)
      .toAccumulator((Person(_, _)).curried)
      .chain(mkAge(age))
  }
}
