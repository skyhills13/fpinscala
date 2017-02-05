package fpinscala.errorhandling.toby

/**
  * Toby
  */

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  /** Ex 4.1 **/
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  // map(Some(x) 대신 x)은 왜 사용할 수 없는지 생각해보자.
  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Test {
  def main (args: Array[String] ): Unit = {
    import Option._
    /** 4.1 **/
    println("Ex4.1: map Some(2) = " + Some(1).map(_+1))
    println("Ex4.1: map None = " + (None:Option[Int]).map(_+1))
    println("Ex4.1: getOrElse 1 = " + Some(1).getOrElse(2))
    println("Ex4.1: getOrElse 2 = " + None.getOrElse(2))
    println("Ex4.1: flatMap Some(1) = " + Some(1).flatMap(Some(_)))
    println("Ex4.1: flatMap None = " + None.flatMap(Some(_)))
    println("Ex4.1: filter Some(1) = " + Some(1).filter(_>0))
    println("Ex4.1: filter None = " + Some(1).filter(_<0))
    println("Ex4.1: filter None = " + None.filter((a:Int) => a<0))
    /** 4.2 **/
    println("Ex4.2: variance Some(1.25) = " + variance(Seq(1,2,3,4)))
    println("Ex4.2: variance None = " + variance(Seq()))
    /** 4.3 **/
    println("Ex4.3: map2 Some(3) = " + map2(Some(1), Some(2))(_+_))
    println("Ex4.3: map2 None = " + map2[Int,Int,Int](None, Some(2))(_+_))
    println("Ex4.3: map2 None = " + map2[Int,Int,Int](Some(1), None)(_+_))
    println("Ex4.3: map2 None = " + map2[Int,Int,Int](None, None)(_+_))
    /** 4.4 **/
    println("Ex4.4: sequence Some(List(1, 2, 3)) = " + sequence(List(Some(1),Some(2),Some(3))))
    println("Ex4.4: sequence None = " + sequence(List(None,Some(2),Some(3))))
    println("Ex4.4: sequence None = " + sequence(List(Some(1),Some(2),None)))
    println("Ex4.4: traverse Some(List(1, 2, 3)) = " + traverse(List(1,2,3))(a => Some(a)))
    println("Ex4.4: traverse None = " + traverse(List(1,2,3))(a => if (a<3) Some(a) else None))

  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** 4.2 **/
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => Some(xs.foldLeft(0.0)((a,b)=>a + math.pow(b-m,2)) / xs.length))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None}

  /** 4.3 **/
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//    a.flatMap(x => b.flatMap(y => Some(f(x,y))))
    a.flatMap(x => b.map(y => f(x,y)))

  /** 4.4 **/
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight(Some(List()):Option[List[A]])((a:Option[A],b:Option[List[A]]) => map2(a,b)((x:A,y:List[A]) => x :: y))
//    a.foldRight(Some(List()):Option[List[A]])((a,b) => map2(a,b)((x,y) => x :: y))
    a.foldRight(Some(List()):Option[List[A]])(map2(_ , _)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()):Option[List[B]])((x,y) => map2(f(x), y)(_ :: _))
}