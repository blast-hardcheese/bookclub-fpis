package week2

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._

object Ch2Specification extends Properties("Ch2") {
  property("fib") = {
    def smallInt: Gen[Int] = Gen.choose(2, 100)
    forAll(smallInt) { n: Int =>
      Ch2.fib(n - 2) + Ch2.fib(n - 1) == Ch2.fib(n)
    }
  }

  property("isSorted") = {
    def sortedArray: Gen[Array[Int]] = {
      val aList = Arbitrary.arbitrary[List[Int]]
      val sortedList = aList.map {
        _.foldLeft(List.empty[Int]) {
          case (a :: acc, x) if x > a => x :: a :: acc
          case (Nil, x) => x :: Nil
          case (acc, _) => acc
        }
      }

      sortedList.map(_.toArray)
    }

    forAll(sortedArray) { xs: Array[Int] =>
      Ch2.isSorted[Int](xs, _ > _)
    }
  }
}
