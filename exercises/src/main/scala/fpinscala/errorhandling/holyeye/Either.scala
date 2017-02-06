package fpinscala.errorhandling.holyeye

import fpinscala.datastructures.holyeye._

/**
  * Created by younghankim on 2017. 2. 6..
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(v) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def map2For[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object EitherMain {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Cons(h, Nil) => h.flatMap(hh => Right(hh).map(ll => Cons(ll, Nil)))
    case Cons(h, t) => h.flatMap(hh => sequence(t).map(ll => Cons(hh, ll)))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    sequence(List.map(as)(f))
  }

  def main(args: Array[String]) = {


    println(mean(IndexedSeq(1, 2, 3)))
    println(mean(IndexedSeq()))
    println(safeDiv(1, 10))
    println(safeDiv(1, 0))
    println(Try(10))
    println(Try(1 / 0))
    //Exercise 4.6
    println("====Exercise 4.6====")
    println(Try(10 / 2).map(a => a.toString) == Right("5"))
    println(Left("bad").map(a => a.toString) == Left("bad"))
    println(Try(10 / 2).flatMap(a => Right(a + 100)) == Right(105))
    println(Right(10).orElse(Right(20)) == Right(10))
    println(Left("bad").orElse(Right(10)) == Right(10))

    def f = ((a: Int, b: Int) => a * b)

    println(Right(10).map2(Right(20))(f) == Right(200))
    println(Right(10).map2For(Right(20))(f) == Right(200))

    println("====Exercise 4.7====")

    println(sequence(List(Right(10), Left("bad"))) == Left("bad"))
    println(sequence(List(Right(10), Right(20))) == Right(Cons(10, Cons(20, Nil))))
    println(traverse(List(1, 2, 3))(a => Try(a * 10)) == Right(Cons(10, Cons(20, Cons(30, Nil)))))
    println(traverse(List(1, 2, 3))(a => Try(a * 10 / 0)).isInstanceOf[Left[String]])

  }


}

object PersonMain {
  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is Emtpy")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    println(mkPerson("hi", 10))
    println(mkPerson("", 10))
    println(mkPerson("hi", -2))
  }
}