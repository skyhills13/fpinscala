package fpinscala.laziness.toby

/**
  * Toby
  */

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /* Ex 5.2
  * Cons와 cons쓰는 것을 비교해보자
  * */
  def take(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h,t) if n > 0 => Cons(h, () => t().take(n-1))
      case Cons(h,t) if n == 0 => Empty
    }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case Cons(h,t) if n == 0 => this
  }

  /* Ex 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  /* Ex 5.5 */
  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a,b) => if (p(a)) cons(a, b) else Empty)

  /* Ex 5.4
  * foldRight는 우측에서부터 계산해오는, foldLeft의 반대가 아니다.
  * */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => if (p(a)) b else false)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /* Ex 5.6  (X)
  * a가 Empty라면 f가 실행되지도 않겠지!
  * */
  def headOptionFR: Option[A] =
    foldRight[Option[A]](None)((a,b) => if (a != Empty) Some(a) else b)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  /* Ex 5.1 */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  /* Ex 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)

  /**
    * (X) Covariant, Contravariant, ...
    * 아래 코드가 왜 안 될까 생각해보자.
    */
  /*def append(s: => Stream[A]): Stream[A] =
    foldRight(s)((a,b) => cons(a,b))*/

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  /* Ex 5.13 */
  def mapUF[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(a, s) => Some((f(a()), s()))
      case Empty => None
    }

  def takeUF(n: Int): Stream[A] =
    unfold(this) {
      case Cons(a, s) if n > 0 => Some((a(), s().take(n-1)))
      case _ => None
    }

  def takeWhileUF(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(a, s) if (p(a())) => Some((a(), s().takeWhile(p)))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(a,b), Cons(c,d)) => Some((f(a(),c()), (b(), d())))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Empty, Cons(c,d)) => Some((None, Some(c())), (Empty, d()))
      case (Cons(a,b), Empty) => Some((Some(a()), None), (b(), Empty))
      case (Cons(a,b), Cons(c,d)) => Some((Some(a()), Some(c())), (b(), d()))
    }

  /* Ex 5.14 */
  def startsWith[A](s: Stream[A]): Boolean =
//    (this, s) match {
//      case (_, Empty) => true
//      case (Empty, _) => false
//      case (Cons(a,b), Cons(c,d)) => if (a() == c()) b().startsWith(d()) else false
//    }
    zipAll(s).takeWhile(_._2 != None).forAll { case (a,b) => a == b}

  /* Ex 5.15 */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(a, b) => Some(Cons(a,b), b())
      case _ => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object LazinessTest {
  def main(args: Array[String]): Unit = {
    import Stream._
    val one23 = Stream(1,2,3)
    val one234 = Stream(1,2,3,4)

    println("*Ex5.1 toList : List(1, 2, 3) = " + one23.toList)

    println("*Ex5.2 take : List(1, 1, 1) = " + ones.take(3).toList)
    println("*Ex5.2 take : List(1, 2) = " + one23.take(2).toList)
    println("*Ex5.2 drop : List(1, 2, 3) = " + one23.drop(0).toList)
    println("*Ex5.2 drop : List(2, 3) = " + one23.drop(1).toList)
    println("*Ex5.2 drop : Empty = " + one23.drop(3))

    println("*Ex5.3 takeWhile : Empty = " + one23.takeWhile(_ > 10))
    println("*Ex5.3 takeWhile : List(1, 2) = " + one23.takeWhile(_ < 3).toList)
    println("*Ex5.3 takeWhile : List(1, 2, 3) = " + one23.takeWhile(_ < 5).toList)

    println("*Ex5.4 foldRight : true = " + one23.forAll(_ > 0))
    println("*Ex5.4 foldRight : false = " + one23.forAll(_ > 1))
    println("*Ex5.4 foldRight : false = " + one23.forAll(_ < 2))

    println("*Ex5.5 takeWhileFR : Empty = " + one23.takeWhileFR(_ > 10))
    println("*Ex5.5 takeWhileFR : List(1, 2) = " + one23.takeWhileFR(_ < 3).toList)
    println("*Ex5.5 takeWhileFR : List(1, 2, 3) = " + one23.takeWhileFR(_ < 5).toList)

    println("*Ex5.6 headOption : Some(1) = " + one23.headOption)
    println("*Ex5.6 headOptionFR : Some(1) = " + one23.headOptionFR)
    println("*Ex5.6 headOption : None = " + Empty.headOption)
    println("*Ex5.6 headOptionFR : None = " + Empty.headOptionFR)

    println("ones: List(1, 1, 1, 1, 1) = " + ones.take(5).toList)

    println("*Ex5.8 constant: List(3, 3) = " + constant(3).take(2).toList)

    println("*Ex5.10 fibs: List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) = " + fibs.take(10).toList)

    println("*Ex5.12 fibsUF: List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) = " + fibsUF.take(10).toList)
    println("*Ex5.12 fromUF: List(2, 3) = " + fromUF(2).take(2).toList)
    println("*Ex5.12 constantUF: List(3, 3, 3) = " + constantUF(3).take(3).toList)
    println("*Ex5.12 onesUF: List(1, 1, 1) = " + onesUF.take(3).toList)

    println("*Ex5.13 mapUF: List(2, 3, 4) = " + one23.mapUF(_ + 1).toList)
    println("*Ex5.13 takeUF: List(1, 2) = " + one23.takeUF(2).toList)
    println("*Ex5.13 takeWhileUF : List(1, 2) = " + one23.takeWhileUF(_ < 3).toList)

    println("*Ex5.13 zipWith : List(2, 3, 4) = " + one23.zipWith(ones)(_ + _).toList)
    println("*Ex5.13 zipAll : List((Some(1),Some(1)), (Some(2),Some(2)), (Some(3),Some(3)), (None,Some(4)))  = " + one23.zipAll(one234).toList)

    println("*Ex5.14 startsWith: true = " + one23.startsWith(Stream(1,2)))
    println("*Ex5.14 startsWith: true = " + one23.startsWith(Stream(1,2,3)))
    println("*Ex5.14 startsWith: false = " + one23.startsWith(Stream(2,3)))

    println("*Ex5.15 tails: List(2, 3, 4) = " + one23.tails.map(_.toList).toList)

  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /* Ex 5.8 */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  /** 아래 방식과 비교 - 하나의 tail을 재사용
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  **/

  /* Ex 5.9 */
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  /* Ex 5.10 */
  def fibs: Stream[Int] = {
    def loop(pre: Int, cur: Int): Stream[Int] = {
      cons(pre, loop(cur, pre + cur))
    }
    loop(0, 1)
  }

  /* Ex 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  /* Ex 5.12 */
  def fibsUF: Stream[Int] = unfold((0,1))(x => Some(x._1, (x._2, x._1 + x._2)))
  /* Cosera 강의에 나오는 case문의 타입 기억해둘 것*/
  /*def fibsUF: Stream[Int] = unfold[Int,(Int,Int)]((0,1)){ case (p,c) => Some(p, (c, p+c)) }*/

  def fromUF(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))
  def constantUF(n: Int): Stream[Int] = unfold(n)(x => Some(x, x))
  def onesUF: Stream[Int] = unfold(1)(x => Some(x, x))


}

























