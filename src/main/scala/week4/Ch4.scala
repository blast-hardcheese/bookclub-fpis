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
}
