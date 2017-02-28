package fpinscala.laziness.toby

import scala.runtime.Nothing$

/**
  * Created by toby on 21/02/2017.
  */
object Variance {
  class Data[+T] {
    def set[U >: T](x: U) { }
  }

  def main(args: Array[String]): Unit = {
    val a: Data[Int] = new Data[Int]
    val b: Data[Int] = new Data[Nothing]
  }
}
