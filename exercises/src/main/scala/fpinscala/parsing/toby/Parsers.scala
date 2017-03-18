package fpinscala.parsing.toby

import language.higherKinds
import fpinscala.testing.answer._
import fpinscala.testing.answer.Prop._

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  /* Ex 9.8 */
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a) // always succeeds with the value a, regardless of the input string
  // LAW: run(succeed(a))(s) == Right(a)

  def slice[A](p: Parser[A]): Parser[String]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  /* Ex 9.6 */
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  /* Ex 9.7 */
  def productFlatMap[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))

  def map2FlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => map(p2)(b => f(a,b)))


  /* Ex 9.1 */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    val prd: Parser[(A, B)] = product(p, p2)
    val ftup: ((A, B)) => C = tup => f(tup._1, tup._2)
//    val ftup2: ((A, B) => C) = { case (a, b) => f(a, b) }
    map(prd)(ftup)

// *  tuple정의와 함수 파라미터 정의가 유사해 보이니 주의해야.
//    val fun1: (Int, Int) => Int = (a, b) => a + b
//    val fun2: ((Int, Int)) => Int = (a) => a._1 + a._2
//    val tuple: (Int, Int) = (1, 2)
//    fun1(tuple) *
//    fun2(tuple)
  }

  // many는 0이 될 수 있으니 1개 이상이 되려면 p를 many에다 무조건 추가한다
  def many1[A](p: Parser[A]): Parser[List[A]] = // one ore more chars
    map2(p, many(p))((a,as) => a :: as)

  /* Ex 9.2 */
  // hmm...

  /* Ex 9.3 */
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))((a,as) => a :: as), succeed(Nil))

  /* Ex 9.4 */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    List.fill(n)(p).foldRight(succeed(Nil:List[A]))((p,z) => map2(p,z)(_ :: _))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  /* Ex 9.5 */
  // ...

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1  // \n 갯수 세기
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')  // 뒤집어서 \n이 나올 때까지의 글자수 세기

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}