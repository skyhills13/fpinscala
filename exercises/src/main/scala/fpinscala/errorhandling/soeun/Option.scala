package fpinscala.errorhandling.soeun

//import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

/*4.1*/
//map과 getOrElse를 제외한 모든 함수는 패턴 매칭 없이 구현할 수 있어야
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  //패턴 매칭을 이용하지 않고 하는 방법을 모르겠음
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

//  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
//    case None => ob
//    case
//  }

//  def filter(f: A => Boolean): Option[A] = this match {
    //왜 안될까
//    case None => None
//    case Some(a) =>
//      if f(a) this
//      else None
//  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a =>
      if(f(a)) Some(a)
      else None
    )
  }
  def main(args: Array[String]): Unit = {
    val x = List(1,2,3)
//    Some(List(1,2,3)).map(x => x + 1)
//    println(Some(x).map(a => a + 1))
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*4.2*/
  //flatmap이용 math.pow(x-m, 2)/n
//  def variance(xs: Seq[Double]): Option[Double] = {
//    if(xs.isEmpty) None
//    else ??? mean(xs.map(x => math.pow(x-m,2))))
//  }
  /*4.3*/
  //74p참고
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(aa =>
      b map(bb =>
        f(aa,bb)
        )
      )
  }
  /*4.4*/
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
//    case Nil => Some(Nil)
//    case h::t =>
//  }
  /*4.5*/
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t =>
  }

  //traverse이용
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
//    case Nil => Some(Nil)
//    case h::t =>
//  }
}