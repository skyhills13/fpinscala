package fpinscala.datastructures.toby

import sun.font.TrueTypeFont

/**
  * Toby
  */
sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = head + "," + tail.toString
}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Ex1. 3 (x+y)
    */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /** Ex 3.2 **/
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) => xs
  }

  /** Ex 3.3 **/
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(x, xs) => Cons(h, xs)
  }

  /** Ex 3.4 **/
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Cons(x, xs) => drop(xs, n - 1)
    }

  /** Ex 3.5 **/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if (f(x)) =>  dropWhile(xs, f)
      case _ => l
    }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if (f(x)) =>  dropWhile(xs, f)
      case _ => l
    }

  /** Ex 3.6 **/
  // l 크기만큼 탐색을 해야하니까
  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(x, xs) => if (xs == Nil) Nil else Cons(x, init(xs))
    }

  /** Ex 3.7 **/
  // 불가능. f에 넘겨지는 파라미터가 이미 재귀호출을 이용하기 때문에 안 됨.

  /** Ex 3.8 **/
  // 같은 구조임

  /** Ex 3.9 **/
  def length[A](l: List[A]): Int = foldRight(l, 0)((_,b) => b+1)

  /** Ex 3.10 **/
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  /** Ex 3. 11 **/
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((b,_) => b+1)

  /** Ex 3.12 **/
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((a,b) => Cons(b, a))

  /** Ex 3.13 **/
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B) =
    foldRight(reverse(l), z)((a,b) => f(b,a))

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B) =
    foldLeft(reverse(l), z)((a,b) => f(b,a))

  /** Ex 3.14 **/
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((b,a) => Cons(a, b))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  /** Ex 3.15 **/
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append(_, _))

  /** Ex 3.16 **/
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((a,b) => Cons(a+1, b))

  /** Ex 3.17 **/
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((a,b) => Cons(a.toString, b))

  /** Ex 3.18 **/
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((a,b) => Cons(f(a), b))

  /** Ex 3.19 **/
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a,b) => if (f(a)) Cons(a, b) else b)

  /** Ex 3.20 **/
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /** Ex 3.21 **/
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  /** Ex 3.22 **/
  def add(la: List[Int], lb: List[Int]): List[Int] =
    (la, lb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(ah+bh, add(at, bt))
    }

  /** Ex 3.23 **/
  def zipWith[A](la: List[A], lb: List[A])(f: (A,A) => A): List[A] =
    (la, lb) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }

  /** Ex 3.24 **/
  // ????
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(ph, pt), Cons(sh, st)) =>
        if (ph == sh) hasSubsequence(pt, st) else hasSubsequence(pt, sub)
    }
}

object Test {
  def main(args: Array[String]): Unit = {
    import List._
    /* 3.2 */ println("ex3.2: " + tail(List(1,2,3,4)))   // 2,3,4
    /* 3.3 */ println("ex3.3: " + setHead(List(1,2,3,4), 0)) // 0,2,3,4
    /* 3.4 */
    println("ex3.4: " + drop(List(1,2,3,4), 1))  // 2,3,4
    println("ex3.4: " + drop(List(1,2,3,4), 4))  // Nil
    /* 3.5 */
    println("ex3.5: " + dropWhile[Int](List(1,2,3,4), _ < 2))  // 2,3,4
    println("ex3.5: " + dropWhile[Int](List(1,2,3,4), _ < 4))  // 4
    println("ex3.5: " + dropWhile[Int](List(1,2,3,4), _ < 10))  // Nil
    println("ex3.5: " + dropWhile2(List(1,2,3,4))(_ < 2)) // 2,3,4
    /* 3.6 */
    println("ex3.6: " + init(List(1,2,3,4)))  // 1.2,3
    /* 3.9 */
    println("ex3.9: " + length(List(1,2,3,4)))  // 4
    /* 3.10 */
    println("ex3.10: " + foldLeft(List(1,2,3,4), 0)(_ + _))  //
    /* 3.11 */
    println("ex3.11: " + sum3(List(1,2,3)))  // 6
    println("ex3.11: " + product3(List(1,2,3)))  // 6
    println("ex3.11: " + length3(List(1,2,3)))  // 3
    /* 3.12 */
    println("ex3.12: " + reverse(List(1,2,3)))  // 3,2,1
    /* 3.13 */
    println("ex3.13: " + foldLeft2(List(1,2,3),0)(_ + _)) // 6
    println("ex3.13: " + foldLeft2(List(2,3,4),1)(_ + _ * 2)) // 19
    println("ex3.13: " + foldRight2(List(1,2,3),0)(_ + _)) // 6
    println("ex3.13: " + foldRight2(List(2,3,4),1)(_ + _ * 2)) // 32
    /* 3.14 */
    println("ex3.14: " + append2(List(1,2,3),List(4,5,6))) // 1,2,3,4,5,6
    println("ex3.14: " + append3(List(1,2,3),List(4,5,6))) // 1,2,3,4,5,6
    /* 3.15 */
    println("ex3.15: " + concat(List(List(1), List(2,3,4), List(5,6)))) // 1,2,3,4,5,6
    /* 3.16 */
    println("ex3.16: " + add1(List(1,2,3))) // 2,3,4
    /* 3.17 */
    println("ex3.17: " + doubleToString(List(1,2,3.4))) // 1,2,3.4
    /* 3.18 */
    println("ex3.18: " + map(List(1,2,3))(_ * 2))   // 2,4,6
    /* 3.19 */
    println("ex3.19: " + filter(List(1,2,3,4))(a => a % 2 == 0))  // 2,4
    /* 3.20 */
    println("ex3.20: " + flatMap(List(1,3))(a => List(a, a+1))) // 1,2,3,4
    /* 3.21 */
    println("ex3.21: " + filter2(List(1,2,3,4))(a => a % 2 == 0))  // 2,4
    /* 3.22 */
    println("ex3.22: " + add(List(1,2), List(3,4))) // 4, 6
    /* 3.23 */
    println("ex3.23: " + zipWith(List(1,2), List(3,4))(_+_)) // 4, 6
    /* 3.24 */
    println("ex3.24: " + hasSubsequence(List(1,2,3,4), List(3,4)))  // true
    println("ex3.24: " + hasSubsequence(List(1,2,3,4), List(2,3)))  // true
    println("ex3.24: " + hasSubsequence(List(1,2,3,4), List(1,2,3,4)))  // true
    println("ex3.24: " + hasSubsequence(List(1,2,3,4), List(1,2,3,5)))  // false
    println("ex3.24: " + hasSubsequence(List(1,2,3,4), List(5,1,2,3)))  // false

  }
}


