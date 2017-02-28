package fpinscala.laziness.holyeye

import fpinscala.laziness.holyeye.Stream._

/**
  * Created by younghankim on 2017. 2. 20..
  */
trait Stream[+A] {


  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * Cons(1,Cons(2, Cons(3, Nil))
    * foldRight(Cons(1,Cons(2, Cons(3, Nil)))(false)(a + b)
    * Cons(1,Cons(2, Cons(3, Nil)))(false)(p(a) || b)
    * f(1, Cons(2, Cons(3, Nil))(false)(a+b) => 1 + f(Cons(2, Cons(3, Nil))
    * f(1, f(2, Cons(3, Nil)))(false)(a+b)
    * f(1, f(2, f(3 + 0)))
    * (1 + (2 + (3+0))
    */

  /**
    * Cons(1,Cons(2, Cons(3, Nil))
    * foldRight(Cons(1,Cons(2, Cons(3, Nil)))(false)(p(a) || b)
    * Cons(1,Cons(2, Cons(3, Nil)))(false)(p(a) || b)
    * f(1, Cons(2, Cons(3, Nil))(false)(p(a) || b)) => (p(1) || Cons(2, Cons(3, Nil))(false)(p(a) || b))
    * (p(1) || (f(Cons(2, Cons(3, Nil))(false)(p(a) || b)))
    * (p(1) || (p(2) || (f(Cons(3, Nil))(false)(p(a) || b)))
    * (p(1) || (p(2) || (p(3) || (false)(p(a) || b)))
    * (p(1) || (p(2) || (p(3) || false)))
    */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
  //    foldRight(true)((a,b) => if(p(a)) b else false)
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty())

  def headOption(): Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](a2: => Stream[B]): Stream[B] =
    foldRight(a2)((h, t) => cons(h, t))

  //  def append(a2: => Stream[A]): Stream[A] =
  //    foldRight(a2)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  //unfold
  def mapViaUnfold[B](f: A => B): Stream[B] =
  //    unfold(empty[B])((x) => Some( (f(x), x) )) //실패
  //    unfold(this)((x) => Some((f(x.headOption().get), x.drop(1)))) //실패
    unfold(this)(x => x match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
    )

  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(_, _), 0) => None
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileVidUnfold(f: A => Boolean) = {
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  }

/*
  def zipWith(l: Stream[A])(f: (A, A) => A): Stream[A] =
    unfold((this, l)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
*/
  def zipWith[B,C](l: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, l)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {


  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    //    Cons(() => {println("h:"+h); h}, () => {println("t:" + t);t})
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A](): Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(i: Int): Stream[Int] = cons(i, from(i + 1))

  def fibs(): Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] = {
      cons(x, go(y, x + y))
    }

    go(0, 1)
  }

  //unfold(0)((1) => Some(1, 0))
  def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(v: Option[(A, S)]): Stream[A] = {
      if (v.isEmpty) empty()
      else cons(v.head._1, go(f(v.head._2)))
    }

    go(f(z))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty()
    }
}

object StreamTest {
  def main(args: Array[String]): Unit = {
    println(cons(1, cons(2, empty())))
    println(Stream(1, 2))
    println("ex5.1: toList" + Stream(1, 2).toList)
    println("ex5.2: take" + Stream(1).take(1).toList)
    println("ex5.2: take" + Stream(1).take(2).toList)
    println("ex5.2: take" + Stream(1, 2, 3, 4).take(2).toList)
    println("ex5.2: take" + Stream(1, 2, 3, 4, 5).take(3).toList)
    println("ex5.2: drop" + Stream(1, 2, 3, 4, 5).drop(3).toList)
    println("ex5.3: takeWhile" + Stream(1, 2, 3, 4, 5).takeWhile(x => x != 3).toList)
    println("ex5.3: exists: " + Stream(1, 2, 3, 4, 5).exists(x => x == 3))
    println("ex5.3: exists: " + Stream(1, 2, 3, 4, 5).exists(x => x == 6))
    println("ex5.3: foldRight: " + Stream(1, 2, 3, 4).foldRight(0)((x, y) => {
      println("x:" + x + " y:" + y);
      x + y
    }))
    println("ex5.3: exist2: " + Stream(1, 2, 3, 4).exists(x => {
      println("x:" + x);
      x == 3
    }))
    println("ex5.3: exist2: " + Stream(1, 2, 3, 4).exists2(x => {
      x == 3
    }))

    val x = 4
    println(({
      println("a");
      x == 3
    } || ({
      println("b");
      x == 4
    } || {
      println("c");
      x == 5
    })))

    println("ex5.4: forall: " + Stream(1, 2, 3, 4).forAll(_ <= 1))
    println("ex5.4: forall: " + Stream(1, 2, 3, 4).forAll(_ <= 4))
    println("ex5.4: forall: " + Stream(1, 2, 3, 4).forAll(_ >= 1))
    println("ex5.5: takeWhile2: " + Stream(1, 2, 3, 4, 5).takeWhile2(x => x != 3).toList)
    println("ex5.6: headOption: " + Stream(1, 2).headOption().get)
    println("ex5.6: headOption: " + Stream().headOption())

    println("ex5.7: map: " + Stream(1, 2).map(_ * 10).toList)
    println("ex5.7: filter: " + Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList)
    println("ex5.7: append: " + Stream(1, 2, 3).append(Stream(4, 5)).toList)
    println("ex5.7: flatMap: " + Stream(1, 2, 3).flatMap((x) => Stream(x * 10)).toList)

    val infiniteOne = Stream.constant(1)
    println("infinite ones :" + infiniteOne)
    println("infinite ones :" + infiniteOne.take(5).toList)
    println("infinite ones :" + Stream.constant(2).take(10).toList)

    println("ex5.9: " + Stream.from(1).take(10))
    println("ex5.9: " + Stream.from(3).take(10).toList)
    println("ex5.10: " + Stream.fibs().take(10).toList)
    println("ex5.11: " + Stream.unfold(1)((x) => Option(x + 1, x + 2)))
    println("ex5.11: " + Stream.unfold(1)((x) => Option(x + 1, x + 2)).take(5).toList)
    println("ex5.12: unfold: fibs=" + Stream.unfold((0, 1))(n => Some(n._1, (n._2, n._1 + n._2))).take(5).toList)
    println("ex5.12: unfold: from=" + Stream.unfold(2)(n => Some((n, n + 1))).take(10).toList);
    println("ex5.12: unfold: constant=" + Stream.unfold(5)(n => Some(n, n)).take(10).toList);
    println("ex5.12: unfold: ones=" + Stream.unfold(1)(n => Some(n, n)).take(10).toList);
    println("ex5.13: map: " + Stream(1, 2, 3).mapViaUnfold(_ * 10).toList)
    println("ex5.13: take: " + Stream(1, 2, 3, 4, 5).takeViaUnfold(3).toList)
    println("ex5.13: take: " + Stream(1, 2).takeViaUnfold(3).toList)
    println("ex5.13: takeWhile: " + Stream(1, 2, 3, 4, 5).takeWhileVidUnfold(_ < 3).toList)
    println("ex5.13: zipWith: " + Stream(1,2,3).zipWith(Stream(4,5,6))((x,y) => x + y).toList)
  }
}

/*
* Error:(157, 44) forward reference extends over definition of variable ones
    var ones: Stream[Int] = Stream.cons(1, ones)
*
 *  */