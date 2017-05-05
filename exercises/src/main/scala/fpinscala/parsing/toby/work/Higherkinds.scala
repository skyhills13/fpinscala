package fpinscala.parsing.toby.work

/**
  * Created by toby on 2017-03-21.
  */

  /**
    * 1. Value: "String", Name("xxx")
    * 2. First-order function: (a: Int) => a + 1   ==> Function, Value Constructor   v -> v.
    * 3. Higher-order function: (a: Int => Int) => a(1)   ==> Function.  f(vc) -> v.
    *
    * 4. Type: Int, String, List[Int], Int => Int
    * 5. Type Constructor:  *(type) -> *(type)    List[A], Either[A,B]  * -> * -> *  : first-kinded type.
    * 6. Higher-kinded Type: (* -> *) -> *    X[A[_]]
    *
    * trait Parsers[Parser[_]]
    */

trait Counter[F[_]] {
  def count(f: F[_]): Int
  def print(f: F[_]) = println(count(f))
}


object Higherkinds {
  implicit object listCounter extends Counter[List] {
    def count(f: List[_]): Int = f.size
  }

  implicit object optionCounter extends Counter[Option] {
    def count(f: Option[_]): Int = f match {
      case Some(_) => 1
      case _ => 0
    }
  }

  implicit class CounterOps[F[_]](f: F[_])(implicit c: Counter[F]) {
    def printCount = c.print(f)
  }

  def main(args: Array[String]): Unit = {
    List(1,2,3).printCount
    List("a","b").printCount
    List().printCount
    (Some("x"):Option[_]).printCount
    (None:Option[_]).printCount
  }
}
