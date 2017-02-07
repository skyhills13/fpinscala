package fpinscala.errorhandling.soeun

//import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

/*4.6*/
//되게 기계적으로 풀었음 => 이해를 못한 것 같다는 뜻
sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => Right(a)
   case Left(_) => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = sys.error("todo")
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]
/*4.7*/
object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = sys.error("todo")

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

  /*4.8*/
  //두 오류를 모두 보고하게 하려면?
  //새로운걸 만드는 게 덜 복잡하겠지만 명확한 근거가 엄슴
}

object Test {
  def main(args: Array[String]): Unit = {
    println("===map===")
    println(Right(1).map(a => a + 1))
    println(Left(1).map(a => 1))
    println(None.map(a => 1))

    println("===flatmap===")
    println(Right(1).flatMap(a => Right(a + 1)))
    println(Left(1).flatMap((a => Left(0))))
    //None 테스트

    println(Right(1).orElse(Right(4)))
//    println(Left(1).orElse())
//    println(None.orElse())
  }
}