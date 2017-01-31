package fpinscala.datastructures.holyeye

import fpinscala.datastructures.holyeye.List.{product, product2, sum}

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

class A

class B extends A

object List {


  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
    *
    * List(1,2)
    * foldRight(Cons(1,Cons(2,Nil)), Nil)((x,y) => ?)
    * Cons(2, foldRight(Cons(2, Nil), Nil)((x,y) => ?))
    * Cons(2, Cons(3, foldRight(Nil, Nil)((x,y) => ?))
    * Cons(2, Cons(3, Nil))
    */
  //Exercise 3.16
  def oneAdded(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((x, y) => Cons(x + 1, y))

  //Exercise 3.17
  def doubleToString(as: List[Double]) = foldRight(as, Nil: List[String])((x, y) => Cons(x.toString(), y))

  //Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  //Exercise 3.20

  /**
    * List(1,2)
    * foldRight(Cons(1, Cons(2, Nil)), Nil)((x,y) => ?)
    * Cons(1, Cons(1, foldRight(Cons(2, Nil), Nil)((x,y) => ?)
    * Cons(1, Cons(1, Cons(2, Cons(2, Nil))
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((x, y) => appendRight(f(x), y))

  //Exercise 3.22
  def matchSum(as1: List[Int], as2: List[Int]): List[Int] = ???


  //Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2[A, B](ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  //Exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * 풀이과정
    * List.sumLeft(1,2,3)
    * foldLeft(Cons(1, Cons(2, Cons(3, Nil), 0) f((y, x) => y + x)
    * foldLeft(Cons(2, Cons(3, Nil), 0) + f(0 + 1))
    * foldLeft(Cons(3, Nil), 0) + f(f(0 + 1) + 2))
    * foldLeft(0 + f(f(f(0 + 1) + 2) + 3)))
    */
  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)((y, x) => y + x)

  def productLeft(ns: List[Int]) = foldLeft(ns, 1.0)(_ * _)

  def reverse(ns: List[Int]) = foldLeft(ns, Nil: List[Int])((a, b) => foldRight(Cons(b, a), Nil: List[Int])(Cons(_, _)))

  //  List.foldRight(List(1, 2, 3), Nil: List[Int])((a, b) => Cons(a, b))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Exercise 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  //Exercise 3.3
  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, tail) => Cons(newHead, tail)
  }

  //Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) =>
      if (n == 1) tail
      else drop(tail, n - 1)
  }

  //Exercise 3.5
  def dropwhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) =>
        if (f(h)) dropwhile(t, f)
        else l
    }

  def dropwhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropwhile2(t)(f)
      case _ => as
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  //Exercise 3.14
  def appendRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  //Exercise 3.6 //Cons가 (head, 꼬리 리스트)를 받는 구조여서 상수 시간으로 구현할 수 없다.
  //이 구조는 꼬리값을 유지하는 리스트는 가능하지만, head 값을 유지하는 리스트를 계속 넘길 수는 없다.
  def init[A](l: List[A]): List[A] = {

    //    def loop(a: A, accum: List[A]): List[A] = {
    //      List(a, accum: _*)
    //    }
    //
    //    case Nil => Nil
    //    case Cons(x, tail) =>
    //      if (tail == Nil) l
    //      else Cons(head, tail)

    null
  }


  def main(args: Array[String]): Unit = {
    val list = Cons(1, Cons(2, Nil));
    println(list)
    println(List(1, 2, 3))
    println(sum(List(1, 2, 3, 4)))
    println(product(List(1, 2, 3, 4)))

    val ex1: List[Int] = Cons(1, Nil)
    val ex2: List[Int] = Nil
    val ex3: List[A] = List(new B(), new B())
    //    val ex4: List[B] = List(new A(), new A())

    //ex 3.2
    println("====ex3.2====")
    println(tail(Nil) == Nil)
    println(tail(List(1, 2, 3)) == List(2, 3))

    //ex 3.3
    println("====ex3.3====")
    println(setHead(1, Nil) == List(1))
    println(setHead(7, List(1, 2, 3)) == List(7, 2, 3))
    println(setHead(10, List(3, 4, 5)))

    //ex 3.4
    println("====ex3.4====")
    println(drop(List(1, 2), 2) == Nil)
    println(drop(List(1, 2, 3), 1) == List(2, 3))
    println(drop(List(1, 2, 3), 2) == List(3))

    println("====ex3.5====")
    println(dropwhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) == List(4, 5))

    println("==== append ====")
    println(append(List(1, 2), List(3, 4)))

    println("====ex3.6====")
    println(init(List(1, 2, 3)) == List(1, 2))

    println("=== dropwhile curry ===")
    val xs: List[Int] = List(1, 2, 3, 4, 5)
    val ee1 = dropwhile2(xs)(x => x < 4)

  }

}

//Exercise 3.1
object Exercise3_1 {
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }
}

object Exercise3_7_RightFold {
  def main(args: Array[String]): Unit = {
    println("=== ex3.7 ===")
    println(product(List(1, 2, 0.0)))
    println(product2(List(1, 2, 0.0)))

    println("=== ex3.8 ===")
    val ex38Result = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(ex38Result)

    val ex38fold = List.foldRight(List(1, 2, 3), Nil: List[Int])(_)
    println(ex38fold((a, b) => Cons(a, b)))

    println("=== ex3.9 ===")
    println(List.length(List(1, 2, 3)))
    println(List.length(List(1, 2, 3)) == 3)
  }
}

object Exercise3_10_FoldLeft {
  def main(args: Array[String]): Unit = {
    println("=== ex3.10 ===")
    println(List.sumLeft(List(1 to 100: _*)) == 5050) //스텍에 안전하지 않다.
    //    println(List.sumLeft(List(1 to 10000: _*))) //스텍에 안전하지 않다.

    println("=== ex3.11 ===")
    println(List.sum(List(1, 2, 3, 4)) == List.sumLeft(List(1, 2, 3, 4)))
    println(List.product(List(1, 2, 3, 4)) == List.productLeft(List(1, 2, 3, 4)))

    println("=== ex3.12 ===")
    println(List.reverse(List(1, 2, 3)))
    println(List.reverse(List(1, 2, 3)) == List(3, 2, 1))

    println("=== ex3.13 === ???")

    println("=== ex3.14 === ")
    println(List.appendRight(List(1, 2, 3), List(4, 5)) == List.append(List(1, 2, 3), List(4, 5)))

    println("=== ex3.15 === ???")

  }
}

object Exercise3_16_FoldLeft {
  def main(args: Array[String]): Unit = {
    println("=== ex3.16 ===")
    println(List.oneAdded(List(1, 2, 3)) == List(2, 3, 4))

    println("=== ex3.17 ===")
    println(List.doubleToString(List(1.0, 2.0)) == List("1.0", "2.0"))

    println("=== ex3.18 ===")
    println(List.map(List(1, 2, 3))((x) => x + 3) == List(4, 5, 6))

    println("=== ex3.19 ===")
    println(List.filter(List(1, 2, 3, 4))(x => x % 2 == 0) == List(2, 4))

    println("=== ex3.20 ===")
    println(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))

    println("=== ex3.21 ===")
    println(List.flatMap(List(1, 2, 3, 4))(i => if (i % 2 == 0) List(i) else Nil) == List(2, 4))

    println("=== ex3.22 === FAIL")
    println(List.matchSum(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }
}
