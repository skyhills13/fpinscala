package fpinscala.errorhandling.holyeye

import fpinscala.datastructures.holyeye._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  } //PatternMatching

  def flatMap[B](f: A => Option[B]): Option[B] = None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  } //PatternMatching

  def orElse[B >: A](ob: => Option[B]): Option[B] = ob

  def filter(f: A => Boolean): Option[A] = None

}

case class Some[+A](get: A) extends Option[A] {

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  override def filter(f: A => Boolean): Option[A] = if (f(get)) this else None

}

case object None extends Option[Nothing]

object Option_4_1 {

  def main(args: Array[String]): Unit = {
    println(Some(1).map(x => x * 2))
    println(Some(1).map(x => x * 2).filter(_ == 100))
    println(Some(1).map(x => x * 2).filter(_ == 2))
    println(Some(1).map(x => x * 2).filter(_ == 100).getOrElse(10) == 10)
    println(Some(1).map(x => x * 2).filter(_ == 2).getOrElse(10) == 2)
    println(Some(1).orElse(Some(2)))
    println(None.orElse(Some(2)))
    println(Some(1).flatMap(x => Some(x + 10)))
    println(Some(1).flatMap(x => Some(x + 10)))
    println(Some(1).flatMap(x => if (x == 1) None else Some(x)))
  }
}

/**
  * 정답참고
  */
object Option_4_2 {

  def mean(xs: Seq[Double]) = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def main(args: Array[String]): Unit = {
    println(variance(Seq(1, 2, 3)))
  }
}

object OptionLeft {

  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    x => x.map(f)
  }

  def main(args: Array[String]) = {
    val abs0 = lift(math.abs)
    println(abs0(Some(-10)).getOrElse(""))
  }
}

object ParseInsuranceRateQuoteMain {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aValue =>
      b.map(bValue =>
        f(aValue, bValue)
      )
    )

/*
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
*/
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
    //    insuranceRateQuote(optAge, optTickets)

  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age * numberOfSpeedingTickets
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def main(args: Array[String]): Unit = {
    val result = parseInsuranceRateQuote("10", "20")
    println(result)
  }
}

object Option_4_4 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Cons(h, Nil) => h.flatMap(hh => Some(hh).map(ll => Cons(ll, Nil)))
    case Cons(h, t) => h.flatMap(hh => sequence(t).map(ll => Cons(hh, ll)))
  }

  //    def parseInts(a: List[String]): Option[List[Int]] = sequence(a.map(i => Try(i.toInt)))
  def parseInts(a: List[String]): Option[List[Int]] = sequence(List.map(a)(i => Try(i.toInt)))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(List.map(a)(f))
  }

  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
    println(sequence(List(Some(1), None, Some(2))) == None)

    println(scala.List("1").map(i => Try(i.toInt)))
    println(scala.List("a").map(i => Try(i.toInt)))

    println(parseInts(List("1", "2")) == Some(List(1, 2)))
    println(parseInts(List("1", "a")) == None)

    println(traverse(List("1", "2"))(i => Try(i.toInt)) == Some(List(1, 2)))
    println(traverse(List("1", "a"))(i => Try(i.toInt)) == None)

  }
}