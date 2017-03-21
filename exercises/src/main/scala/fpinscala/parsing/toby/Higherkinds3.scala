package fpinscala.parsing.toby.step3

import language.higherKinds
/**
  * Created by toby on 2017-03-21.
  */

trait Counter[F[_]] {
  def count(f: F[_]): Int
  def print(f: F[_]) = println(count(f))
}

object Test {
  implicit object listCounter extends Counter[List] {
    def count(f: List[_]): Int = f.size
  }

  implicit object optionCounter extends Counter[Option] {
    def count(f: Option[_]): Int = f match {
      case Some(_) => 1
      case _ => 0
    }
  }

  implicit class counterOps[F[_]](f: F[_])(implicit c: Counter[F]) {
    def printCount() = c.print(f)
  }

  def printCount[F[+_]](a: F[_])(implicit c: Counter[F]) =
    c.print(a)

  def main(args: Array[String]): Unit = {
    List(1,2,3).printCount
    List().printCount
    List("a","b").printCount
    (Some("X"):Option[_]).printCount
    (None:Option[_]).printCount
  }
}

