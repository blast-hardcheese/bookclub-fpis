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

  property("2: variance") = {
    forAll { xs: Seq[Double] =>
      val answer = if (xs.nonEmpty) {
        val len = xs.length
        val m = xs.sum / len
        Some(xs.map(x => Math.pow(x - m, 2)).sum / len)
      } else None

      Ch4.variance(xs) == answer
    }
  }

  property("3: map2") = {
    forAll { (_o1: ScalaOption[Int], _o2: ScalaOption[Int]) =>
      val o1 = toOption(_o1)
      val o2 = toOption(_o2)
      Ch4.map2(o1, o2)(_ + _) == toOption(_o1.flatMap(x => _o2.map(_ + x)))
    }
  }

  property("4: sequence") = {
    forAll { xs: Seq[ScalaOption[Unit]] =>
      Ch4.sequence(xs.map(toOption).toList) == toOption(ScalaOption(xs.filter(_.nonEmpty).map(_.get).toList).filterNot(_ => xs.exists(_.isEmpty)))
    }
  }
}
