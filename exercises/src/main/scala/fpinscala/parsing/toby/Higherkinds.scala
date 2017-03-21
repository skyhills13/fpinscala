package fpinscala.parsing.toby.step1

import language.higherKinds
/**
  * Created by toby on 2017-03-21.
  */

trait Counter[F[_]] {
  def count(f: F[_]): Int
  def print(f: F[_]) = println(count(f))
}

object listCounter extends Counter[List] {
  def count(f: List[_]): Int = f.size
}

object optionCounter extends Counter[Option] {
  def count(f: Option[_]): Int = f match {
    case Some(_) => 1
    case _ => 0
  }
}

object Test {
  def printCount[F[+_]](a: F[_])(c: Counter[F]) =
    c.print(a)

  def main(args: Array[String]): Unit = {
    printCount(List(1,2,3))(listCounter)
    printCount(List())(listCounter)
    printCount(Some("X"):Option[_])(optionCounter)
    printCount(None:Option[_])(optionCounter)
  }
}

