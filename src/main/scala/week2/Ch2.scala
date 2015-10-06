package week2

object Ch2 {
  def fib(n: Int): Int = {
    val initial = (0, 1)
    val f: (Int, Int) => Int = _ + _

    val (fst, _) = (0 until n).foldLeft(initial) {
      case (x, _) => x.swap.copy(_2 = f.tupled(x))
    }

    fst
  }

  def isSorted[T](as: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int, res: Boolean): Boolean =
      if (! res || (i + 1 >= as.length)) res
      else go(i + 1, ordered(as(i), as(i+1)))

    go(0, true)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def main(args: Array[String]): Unit = {
    println(s"fib(1): ${fib(1)}")
    println(s"fib(2): ${fib(2)}")
    println(s"fib(3): ${fib(3)}")
    println(s"fib(4): ${fib(4)}")
    println(s"fib(5): ${fib(5)}")

    println(s"isSorted: ${isSorted[Int](Array(1,2,3,4,5), _ < _)}")
    println(s"isSorted: ${isSorted[Int](Array(1,3,2,4,5), _ < _)}")
    println(s"isSorted: ${isSorted[Int](Array(0, 1, 4, 2, 3, 5), _ % 2 <= _ % 2)}")

    println(s"curry(1,2): ${curry[Int, Int, Int](_ + _)(1)(2)}")

  }
}
