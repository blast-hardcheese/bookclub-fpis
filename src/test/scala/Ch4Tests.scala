package week4

import scala.{ Option => ScalaOption, Some => ScalaSome, None => ScalaNone }

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._

object Ch4Specification extends Properties("Ch4") {
  def toOption[T](o: ScalaOption[T]): Option[T] = o.map(Some.apply _).getOrElse(None)

  property("1: map") = {
    forAll { o: ScalaOption[Int] =>
      toOption(o).map(_ * 2) == toOption(o.map(_ * 2))
    }
  }

  property("1: flatMap") = {
    forAll { o: ScalaOption[Int] =>
      toOption(o).flatMap(x => if (x % 3 == 0) Some(x) else None) == toOption(o.filter(_ % 3 == 0))
    }
  }

  property("1: getOrElse") = {
    forAll { o: ScalaOption[Int] =>
      toOption(o).getOrElse(100) == o.getOrElse(100)
    }
  }

  property("1: orElse") = {
    forAll { (o1: ScalaOption[Int], o2: ScalaOption[Int]) =>
      toOption(o1).orElse(toOption(o2)) == toOption(o1.orElse(o2))
    }
  }

  property("1: filter") = {
    forAll { o: ScalaOption[Int] =>
      toOption(o).filter(_ % 3 == 0) == toOption(o.filter(_ % 3 == 0))
    }
  }
}
