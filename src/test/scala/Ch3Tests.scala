package week3

import scala.collection.immutable.{ List => ScalaList }
import scala.util.Try

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._

object Ch3Specification extends Properties("Ch3") {
  val smallArbitraryInt: Arbitrary[Int] = Arbitrary(Gen.choose(-1000, 1000))
  def nonEmptyList[T](implicit ev: Arbitrary[ScalaList[T]]) = Arbitrary.arbitrary[ScalaList[T]] suchThat (_.nonEmpty)
  def toList[T](xs: ScalaList[T]): List[T] = xs.foldRight(Nil: List[T])(Cons.apply[T] _)

  case class TreeRepr[T](tree: Tree[T], size: Int, depth: Int, min: Option[T], max: Option[T])
  def genTree[T](min: Option[(T, T) => T], max: Option[(T, T) => T])(implicit ev: Arbitrary[T]): Gen[TreeRepr[T]] = {
    def app(f: Option[(T, T) => T], l: Option[T], r: Option[T]): Option[T] = {
      for {
        _f <- f
        _l <- l
        _r <- r
      } yield _f(_l, _r)
    }

    def genTree(n: Int): Gen[TreeRepr[T]] = n match {
      case 0 => genLeaf
      case n => Gen.oneOf(genLeaf, genNode(n - 1))
    }

    def genLeaf = for {
      x <- Arbitrary.arbitrary[T]
    } yield TreeRepr(Leaf(x), 1, 1, min.map(_ => x), max.map(_ => x))

    def genNode(n: Int) = for {
      TreeRepr(left,  lsize, ldepth, lmin, lmax) <- genTree(n)
      TreeRepr(right, rsize, rdepth, rmin, rmax) <- genTree(n)
    } yield TreeRepr(Branch(left, right), lsize + rsize, Math.max(ldepth, rdepth), app(min, lmin, rmin), app(max, lmax, rmax))

    for {
      depth <- Gen.choose(5, 200)
      tree <- genTree(depth)
    } yield tree
  }

  // 3.1: What will be the result of the following match expression?
  // val x = List(1,2,3,4,5) match {
  //   case Cons(x, Cons(2, Cons(4, _))) => x
  //   case Nil => 42
  //   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
  //   case Cons(h, t) => h + sum(t)
  //   case _ => 101
  // }
  //
  // x + y: 3, due to the prior patterns not matching

  property("2: tail") = {
    forAll(nonEmptyList[Int]) { aList: ScalaList[Int] =>
      val xs = toList(aList)
      val tailxs = toList(aList.tail)

      Ch3.tail(xs) == tailxs
    }
  }

  property("3: setHead") = {
    forAll(nonEmptyList[Int]) { aList: ScalaList[Int] =>
      val xs = toList(aList)
      val setxs = toList(99 +: aList.tail)

      Ch3.setHead(99, xs) == setxs
    }
  }

  property("4: drop") = {
    forAll { (aList: ScalaList[Int], n: Int) =>
      val xs = toList(aList)
      if (aList.length < n) {
        Try({
          Ch3.drop(xs, n)
        }).isFailure
      } else {
        Ch3.drop(xs, n) == toList(aList.drop(n))
      }
    }
  }

  property("5: dropWhile") = {
    forAll { aList: ScalaList[Int] =>
      Ch3.dropWhile(toList(aList)) { _ < 100 } == toList(aList.dropWhile(_ < 100))
    }
  }

  property("6: init") = {
    forAll(nonEmptyList[Int]) { xs: ScalaList[Int] =>
      Ch3.init(toList(xs)) == toList(xs.init)
    }
  }

  // 3.7: Can product, implemented using foldRight, immediately halt the recursion and
  // return 0.0 if it encounters a 0.0? Why or why not?
  //
  // Without a sentinel, no. Providing our own foldRight, we could utilize the following:
  //
  // sealed trait FoldState
  // case object Initial extends FoldState
  // case object Loop extends FoldState
  // case object Terminate extends FoldState
  //
  // used like: xs.foldRight(1.0) { case (state, left, right) =>
  //   ((if (left == 0.0) Terminate else Loop), left * right)
  // }

  // 3.8: See what happens when you pass Nil and Cons themselves to foldRight
  //
  // You get identity, as the data structures are right-associated. foldLeft
  // would give you the opposite, reversing the list.

  property("9: length") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.length(toList(xs)) == xs.length
    }
  }

  property("10: foldLeft") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.foldLeft(toList(xs), 0)(_ + _) == List.sum(toList(xs))
    }
  }

  property("11: foldLeft sum") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.sum(toList(xs)) == xs.sum
    }
  }

  property("11: foldLeft product") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.product(toList(xs)) == xs.product
    }
  }

  property("11: foldLeft length") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.length2(toList(xs)) == xs.length
    }
  }

  property("12: reverse") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.reverse(toList(xs)) == toList(xs.reverse)
    }
  }

  property("13: foldLeft via foldRight") = {
    forAll(nonEmptyList[Int]) { xs: ScalaList[Int] =>
      Ch3.foldLeft2(toList(xs), Nil: List[Int])((a, x) => Cons(x, a)) == toList(xs.reverse)
    }
  }

  property("13: foldRight via foldLeft") = {
    forAll(nonEmptyList[Int]) { xs: ScalaList[Int] =>
      Ch3.foldRight2(toList(xs), Nil: List[Int])(Cons.apply[Int] _) == toList(xs)
    }
  }

  property("14: append in terms of fold*") = {
    forAll { (xs: ScalaList[Int], x: Int) =>
      Ch3.append(toList(xs), x) == toList(xs :+ x)
    }
  }

  property("15: concat") = {
    forAll { (l1: ScalaList[Int], l2: ScalaList[Int], l3: ScalaList[Int], l4: ScalaList[Int]) =>
      val together = l1 ++ l2 ++ l3 ++ l4
      Ch3.concat(ScalaList(l1, l2, l3, l4).map(toList _): _*) == toList(together)
    }
  }

  property("16: increment list") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.incList(toList(xs)) == toList(xs.map(_ + 1))
    }
  }

  property("17: stringify list") = {
    forAll { xs: ScalaList[Double] =>
      Ch3.stringifyList(toList(xs)) == toList(xs.map(_.toString))
    }
  }

  property("18: map") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.map(toList(xs)) { _ % 7 } == toList(xs.map(_ % 7))
    }
  }

  property("19: filter") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.filter(toList(xs)) { _ % 2 == 0 } == toList(xs.filter(_ % 2 == 0))
    }
  }

  property("19: filterAnnotated") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.filterAnnotated(toList(xs)) { _ % 2 == 0 } == toList(xs.filter(_ % 2 == 0))
    }
  }

  property("20: flatMap") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.flatMap(toList(xs)) { x => List(x, x) } == toList(xs.flatMap { x => ScalaList(x, x) })
    }
  }

  property("21: filter via flatMap") = {
    forAll { xs: ScalaList[Int] =>
      Ch3.filter2(toList(xs)) { _ % 2 == 0 } == toList(xs.filter(_ % 2 == 0))
    }
  }

  property("22: zipAdd") = {
    forAll { (xs: ScalaList[Int], ys: ScalaList[Int]) =>
      Ch3.zipAdd(toList(xs), toList(ys)) == toList(xs.zip(ys).map { case (x, y) => x + y })
    }
  }

  property("23: zipWith") = {
    forAll { (xs: ScalaList[Int], ys: ScalaList[Int]) =>
      Ch3.zipWith(toList(xs), toList(ys))(_ + _) == toList(xs.zip(ys).map { case (x, y) => x + y })
    }
  }

  property("24: hasSubsequence") = {
    val genIntList = Gen.containerOf[ScalaList,Int](Gen.oneOf(1, 2, 3, 4, 5))
    val genSmallIntList = Gen.containerOf[ScalaList,Int](Gen.oneOf(1, 2, 3, 4, 5)).map {
      case xs if xs.nonEmpty => xs.take(xs.head)
      case xs => xs
    }
    forAll(genIntList, genSmallIntList) { (xs: ScalaList[Int], ys: ScalaList[Int]) =>
      Ch3.hasSubsequence(toList(xs), toList(ys)) == xs.indexOfSlice(ys) >= 0
    }
  }

  property("25: size") = forAll(genTree[Int](Some(Math.min _), Some(Math.max _))) {
    case TreeRepr(tree, size, depth, min, max) =>

    Ch3.size(tree) == size
  }

  property("26: maximum") = forAll(genTree[Int](Some(Math.min _), Some(Math.max _))) {
    case TreeRepr(tree, size, depth, min, max) =>

    Ch3.maximum(tree) == max.get
  }

  property("27: depth") = forAll(genTree[Int](Some(Math.min _), Some(Math.max _))) {
    case TreeRepr(tree, size, depth, min, max) =>

    Ch3.depth(tree) == depth
  }

  property("28: map") = forAll(genTree[Int](Some(Math.min _), Some(Math.max _))(smallArbitraryInt)) {
    case TreeRepr(tree, size, depth, min, max) =>

    Ch3.maximum(Ch3.map2(tree)(_ * 2)) == Ch3.maximum(tree) * 2
  }

  property("29: fold") = forAll(genTree[Int](Some(Math.min _), Some(Math.max _))(smallArbitraryInt)) {
    case TreeRepr(tree, size, depth, min, max) =>

    Ch3.fold(tree)(_ => 1)(_ + _) == size &&
    Ch3.fold(tree)(identity)(Math.max _) == max.get &&
    Ch3.fold(tree)(_ => 1)(Math.max _) == depth &&
    Ch3.fold[Int, Tree[Int]](tree)(x => Leaf(x * 2))(Branch.apply _) == Ch3.map2(tree)(_ * 2)
  }
}
