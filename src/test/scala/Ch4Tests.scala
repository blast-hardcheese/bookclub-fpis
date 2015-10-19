package week4

import scala.{ Option => ScalaOption, Some => ScalaSome, None => ScalaNone }
import scala.{ Either => ScalaEither, Left => ScalaLeft, Right => ScalaRight }

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

  property("5: sequence2") = {
    forAll { xs: Seq[ScalaOption[Unit]] =>
      Ch4.sequence2(xs.map(toOption).toList) == toOption(ScalaOption(xs.filter(_.nonEmpty).map(_.get).toList).filterNot(_ => xs.exists(_.isEmpty)))
    }
  }

  property("6: Either.map") = {
    forAll { (o2: String, o1: ScalaOption[Int]) =>
      val e = o1.map(Right.apply _).getOrElse(Left(o2))

      val e2 = e.map(_ * 3)

      if (e2.isRight) {
        e2 == Right(o1.get * 3)
      } else {
        e2 == Left(o2)
      }
    }
  }

  property("7: sequence3") = {
    forAll { (xs: List[ScalaEither[String, Int]]) =>
      val res: Either[String, List[Int]] = Right(xs).flatMap { xs => xs.find(_.isLeft).map(x => Left(x.left.get)).getOrElse(Right(xs.map(x => x.right.get))) }
      val es = xs.map(_.fold(Left.apply _, Right.apply _))

      Ch4.sequence3(es) == res
    }
  }

  property("8: mkPerson") = {
    forAll { (name: ScalaOption[String], dangerousAge: Int) =>
      val dangerousName = name.getOrElse(null)

      val nameRes = Ch4.mkName(dangerousName)
      val ageRes = Ch4.mkAge(dangerousAge)

      val errors: Either[List[String], Person] = (nameRes, ageRes) match {
        case (Left(v1), Left(v2)) => Left(List(v1, v2))
        case (Left(v1), _) => Left(List(v1))
        case (_, Left(v2)) => Left(List(v2))
        case (Right(name), Right(age)) => Right(Person(name, age))
      }

      Ch4.mkPerson2(dangerousName, dangerousAge) == errors
    }
  }

  // 8: How would orElse, traverse, and sequence behave differently?
  // Since all of these methods short-circuit in the case of failure,
  // and since there's a possibility of applying a map which changes
  // either Right or Left's type, the error collector is less flexible.
  //
  // Collecting by way of partially applied function works very well,
  // but in order to flatMap to a different type for Left, we'd have to
  // provide L => L2 to convert all the L's up to the new type, which
  // would be a hassle.
  //
  // scala> (Left("first"): Either[String, Int]).toAccumulator(((_: Int) :: (_: Int) :: (_: Int) :: Nil).curried).chain(Left("Second")).chain(Left("Third"))
  // res14: week4.Either[List[String],List[Int]] = Left(List(first, Second, Third))
  //
  // scala> (Left("first"): Either[String, Int]).toAccumulator(((_: Int) :: (_: Int) :: (_: Int) :: Nil).curried).chain(Right(5)).chain(Left("Third"))
  // res15: week4.Either[List[String],List[Int]] = Left(List(first, Third))
  //
  // scala> (Right(1): Either[String, Int]).toAccumulator(((_: Int) :: (_: Int) :: (_: Int) :: Nil).curried).chain(Right(2)).chain(Right(3))
  // res16: week4.Either[List[String],List[Int]] = Right(List(1, 2, 3))
}
