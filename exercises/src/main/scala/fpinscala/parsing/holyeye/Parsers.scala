package fpinscala.parsing.holyeye

import fpinscala.parsing.holyeye.Types.Parser

import scala.util.matching.Regex

/**
  * Created by younghankim on 2017. 3. 15..
  */
trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] =
    string(c.toString()).map(_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  def orString(s1: String, s2: String): Parser[String]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, listOfN(n - 1, p))(_ :: _) or succeed(List()) //ex9.4
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())// 정답참고 (ex9.3)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  //ex 9.1 정답참고
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a,b) => a :: b)
  }

  //ex 9.7
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
  //    p.flatMap(a => p2.map(b => (a,b)))
    for {a <- p; b <- p2} yield (a,b)

  //ex 9.1, 9.7
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  //    product(p, p2).map(a => f(a._1, a._2))
  //    p.flatMap(a => p2.map(b => f(a,b)))
    for {a <- p; b <- p2} yield f(a,b)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def regex(r: Regex): Parser[String]

  //EX 9.6?
  //  def contextSensitive[A,B](p: Parser[A]): Parser[B] =
  //    regex("[0-9]+").flatMap(a => a.map(b => listOfN(b.toInt, char('a')))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)
    def many1 = self.many1(p)
    def slice = self.slice(p)
    def **[B >: A](p2: Parser[B]): Parser[(A,B)] = self.product(p ,p2)
    def product[B](p2: Parser[B]) = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  def test(args: Array[String]): Unit = {
    println("go")
    "hi" | "go"
    run(char('h'))("h") == Right('h')
    run(string("hi"))("hi") == Right("hihi")
    run(orString("hi", "hello"))("hi") == Right("hi")
    run(or(string("hi"), string("hello")))("hi") == Right("hi")
    run(or("hi", "hello"))("hi") == Right("hi")
    run("hi" | "hello")("hi") == Right("hi")
    run("hi")("hi") == Right("hihi")
    "hi" or "hello" | "go"
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right(List("ab","ab","cad"))
    map(many(char('a')))(_.size)
    val numA: Parser[Int] = char('a').many.map(_.size)

    run(numA)("aaa") == Right(3)
    run(numA)("b") == Right(0)

    run(slice(("a"|"b").many))("aaba") == Right("aaba")
    char('a').many.slice.map(_.size)

    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
    run("[0-9]*".r)("0") == Right("0")

  }
}


class MyParser[+A](input: String => Either[MyErr, A]) {

}

case class MyErr(msg: String) {

}

object Types {
  type Parser[+A] = String => Either[MyErr, A]
}

//Ex 9.12
object MyParser extends Parsers[MyErr, Types.Parser] {

  def run[A](p: Parser[A])(input: String): Either[MyErr, A] = {
    p(input)
  }

  implicit def string(s: String): Parser[String] = {
    (input: String) =>
      if (input.startsWith(s))
        Right(s)
      else
        Left(MyErr(s))
  }

  def orString(s1: String, s2: String): Parser[String] = ???

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  implicit def regex(r: Regex): Parser[String] = ???
}

object ParsersTest {
  def main(args: Array[String]): Unit = {
    import MyParser._
    println("ex 9.12: " + MyParser.run("a")("a"))
    println("ex 9.12: " + MyParser.run("a")("b"))
    println("ex 9.12: " + MyParser.run("a".many)("aaa"))
    println("ex 9.12: " + MyParser.run("a".many)("aaab"))
  }
}
