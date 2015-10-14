package week3

import Function.{ uncurried, const }

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def empty[T]: List[T] = Nil
}

trait Ch3Provided {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

object Ch3 extends Ch3Provided {
  def init[T](xs: List[T]): List[T] = xs match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => ???
  }

  @annotation.tailrec
  def last[T](xs: List[T]): T = xs match {
    case Cons(x, Nil) => x
    case Cons(_, xs) => last(xs)
    case Nil => ???
  }

  def tail[T](xs: List[T]): List[T] = xs match {
    case Cons(_, xs) => xs
    case Nil => ???
  }

  def drop[T](xs: List[T], n: Int): List[T] = xs match {
    case Cons(x, xs) if n > 0 => drop(xs, n - 1)
    case Nil if n > 0 => ???
    case xs => xs
  }

  def setHead[T](x: T, list: List[T]): List[T] = list match {
    case Cons(_, xs) => Cons(x, xs)
    case Nil => ???
  }

  def dropWhile[T](xs: List[T])(predicate: T => Boolean): List[T] = xs match {
    case Cons(x, xs) if predicate(x) => dropWhile(xs)(predicate)
    case xs => xs
  }

  def length[T](xs: List[T]): Int = {
    foldRight(xs, 0) { case (_, a) => a + 1 }
  }

  @annotation.tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
  }

  def sum(xs: List[Int]): Int = foldLeft(xs, 0) { _ + _ }
  def product(xs: List[Int]): Int = foldLeft(xs, 1) { _ * _ }
  def length2(xs: List[Int]): Int = foldLeft(xs, 0) { case (a, _) => a + 1 }

  def reverse[T](xs: List[T]): List[T] = foldLeft(xs, Nil: List[T]) { case (a, x) => Cons(x, a) }

  def foldLeft2[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
    (foldRight(xs, identity[B] _) { case (x, a) => (f(_: B, x)) andThen a })(z)

  def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    (foldLeft2(xs, identity[B] _) { case (a, x) => (f(x, _: B)) andThen a })(z)

  def append[T](xs: List[T], x: T): List[T] = foldRight2(xs, List(x))(Cons(_, _))

  def concat[T](lists: List[T]*): List[T] = {
    val xss = List(lists: _*)
    foldRight2(init(xss), last(xss)) { case (xs, a) =>
      foldRight2(xs, a)(Cons(_, _))
    }
  }

  def incList(xs: List[Int]): List[Int] = foldRight2(xs, List.empty[Int])(uncurried(((_: Int) + 1) andThen (Cons.apply[Int] _).curried))

  def stringifyList(xs: List[Double]): List[String] = foldRight2(xs, List.empty[String])(uncurried(((_: Double).toString) andThen (Cons.apply[String] _).curried))

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight2(as, List.empty[B])(uncurried(f andThen (Cons.apply[B] _).curried))

  def split[A, B, C](f: A => A => C)(x: A): C = f(x)(x)
  def swap[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, List.empty[A])(uncurried[A, List[A], List[A]](split(f andThen (if(_) (Cons.apply[A] _).curried else swap(const[List[A], A] _))) _))
  def filterAnnotated[A](as: List[A])(f: A => Boolean): List[A] = {
    val prepend: A => List[A] => List[A] = (Cons.apply[A] _).curried
    val ignore:  A => List[A] => List[A] = swap(const[List[A], A] _)

    val getApply: A => A => List[A] => List[A] = f andThen (if(_) prepend else ignore)

    val replicated:    A => List[A] => List[A] = split(getApply) _
    val foldBody: (A, List[A]) => List[A] = uncurried(replicated)

    foldRight2(as, List.empty[A])(foldBody)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight2(as, List.empty[B])(uncurried(f andThen (concat(_: List[B], _: List[B])).curried))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) { x => if (f(x)) Cons(x, Nil) else Nil }

  def zipAdd(xs: List[Int], ys: List[Int]) = {
    @annotation.tailrec
    def iter(xs: List[Int], ys: List[Int])(acc: List[Int] => List[Int]): List[Int] => List[Int] =
      (xs, ys) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => iter(xs, ys)((Cons(x + y, _: List[Int])) andThen acc)
      }

    iter(xs, ys)(identity[List[Int]])(Nil)
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def iter(xs: List[A], ys: List[B])(acc: List[C] => List[C]): List[C] => List[C] =
      (xs, ys) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => iter(xs, ys)((Cons(f(x, y), _: List[C])) andThen acc)
      }

    iter(xs, ys)(identity[List[C]])(Nil)
  }

  def hasSubsequence[T](xs: List[T], ys: List[T]): Boolean = {
    val yis = foldLeft(ys, Map.empty[Int, T]) { case (a, x) => a.updated(a.size, x) }

    @annotation.tailrec
    def iter(xs: List[T], offsets: Set[Int]): Boolean = {
      xs match {
        case Nil => false
        case Cons(x, xs) =>
          val nextOffsets = (offsets.map(_ + 1) + 0).filter(yis.get(_).exists(_ == x))

          if (nextOffsets.contains(length(ys) - 1)) {
            true
          } else {
            iter(xs, nextOffsets)
          }
      }
    }

    if (yis.nonEmpty) {
      iter(xs, Set.empty)
    } else {
      true
    }
  }

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => Math.max(maximum(l), maximum(r))
  }

  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => Math.max(depth(l), depth(r))
  }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map2(l)(f), map2(r)(f))
  }

  def fold[A, B](tree: Tree[A])(a: A => B)(b: (B, B) => B): B = tree match {
    case Leaf(x) => a(x)
    case Branch(l, r) => b(fold(l)(a)(b), fold(r)(a)(b))
  }
}
