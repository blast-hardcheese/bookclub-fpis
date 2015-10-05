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

  def main(args: Array[String]): Unit = {
    println(s"fib(1): ${fib(1)}")
    println(s"fib(2): ${fib(2)}")
    println(s"fib(3): ${fib(3)}")
    println(s"fib(4): ${fib(4)}")
    println(s"fib(5): ${fib(5)}")
  }
}
