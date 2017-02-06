package fpinscala.errorhandling.toby

/**
  * Toby
  */

import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  /** Ex 4.6 **/
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(x) => Right(f(x))
   case x @ Left(e) => x  // 그냥 @ 한번 써보고 싶어서...
 }

  // EE >: E를 사용한 이유(E+)에 대해서 설명해보자
 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(x) => f(x)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(x) => Right(x)
   case Left(_) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   flatMap(x => b.map(f(x,_)))
//  for {
//    x <- this
//    y <- b
//  } yield f(x,y)

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Test2 {
  def main(args: Array[String]): Unit = {
    import Either._
    /** 4.6 **/
    println("Ex4.6: map Right(2) = " + Right(1).map(_+1))
    println("Ex4.6: map Left(1) = " + Left(1).map(_=>Left(2)))
    println("Ex4.6: flatMap Right(2) = " + Right(1).flatMap(a=>Right(a+1)))
    println("Ex4.6: flatMap Left(1) = " + Left(1).flatMap(Right(_)))
    println("Ex4.6: orElse Right(1) = " + Right(1).orElse(Right(3)))
    println("Ex4.6: orElse Right(3) = " + Left(1).orElse(Right(3)))
    println("Ex4.6: map2 Right(3) = " + Right(1).map2(Right(2))(_+_))
    println("Ex4.6: map2 Left(1) = " + Left(1).map2(Right(2))((_,a)=>a))
    println("Ex4.6: map2 Left(2) = " + Right(1).map2(Left(2))((_,a)=>a))
    /** 4.7 **/
    println("Ex4.7: traverse Right(List(1, 2, 3)) = " + traverse[Int,Int,Int](List(1,2,3))(Right(_)))
    println("Ex4.7: traverse Left(2) = " + traverse[Int,Int,Int](List(1,2,3))(x=>if (x==2) Left(x) else Right(x)))
    println("Ex4.7: sequence Right(List(1, 2, 3)) = " + sequence(List(Right(1),Right(2),Right(3))))
    println("Ex4.7: sequence Left(2) = " + sequence(List(Right(1),Left(2),Right(3))))
  }
}

object Either {
  /** Ex 4.7 **/
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(List()):Either[E, List[B]])((x,y) => f(x).map2(y)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /** Ex 4.8 **/
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}

