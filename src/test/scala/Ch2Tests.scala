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
}
