package fpinscala.errorhandling.soeun

//import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
/*4.1*/
//map과 getOrElse를 제외한 모든 함수는 패턴 매칭 없이 구현할 수 있어야
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /*(default: => B) 는 (default: B) 일 수도 있었어.
    하지만 이렇게 되면 None일 아니고 Some(3)일 때도 default 값을 계산하는 낭비가 발생함
    그래서 실제로 B가 사용되기 전까지는 평가되지 않는다는 의미에서 (default: {} => B)를 한 것.
    주로 성능 향상을 시킬 때 사용해
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /*flatMap을 사용하면, a에 f라는 함수를 적용하는데, f는 Option[B]를 반환해.
    그러면 Option[A] 값에 그냥 f를 적용하면 Option[Option[B]]가 나오는데, flatMap을 적용해서 Option[B]가 나오게되는거야
    그러면 (f : a => Some(a + 1)) 라고 할 때
    Some(1)에 f를 적용하면 원래 Some(Some(2))가 나오지만
    flatMap을 적용하면 Some(2)가 나오는거야
  */
  // Some(1) f: a => Some(a + 1)  Some(Some(2)) -> Some(2), None
  // 패턴 매칭을 이용하지 않고 하는 방법을 모르겠음
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  //어제 했는데 기억 왜 못하지 바본가봐 TODO 뭐 잘못되었는지 찾아
//  def flatMap1[B](f: A => Option[B]): Option[B] =
//    map(f).getOrElse(a)

  // Some(1) => Some(Some(1)) => getOrElse() => Some(1)
  // None => None => getOrElse() => ob
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  // None => None
  // Some(1) => Some(1)
  // Some(2) => None
  def filter1(f: A => Boolean): Option[A] =
//    this match {
//      case None => None
//      case Some(a) =>
//        if (f(a)) this
//        else None
//    }
    flatMap(a => if (f(a)) Some(a) else None)

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a =>
      if (f(a)) Some(a)
      else None
    )
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*4.2*/
  //flatmap이용 math.pow(x-m, 2)/n
  def variance(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
  }
  /*4.3*/
  //74p참고
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
//    a.map(aa => {b.map(bb => f(aa,bb))})
//    a.map(aa => {b.map(aa+bb)})
//    a.map(aa => {Some(aa+bb)})
//    Some(Some(aa+bb)))
//
//    a.map(aa => b.map(bb =>f(aa, bb)))
//    a.map(aa => Some(f(aa, bb)))
//    Some(Some(f(aa, bb))))
//
//    a.flatMap(aa => Option(f(aa, bb)))
//    Option(f(aa, bb))
//
//    a.flatMap(a => Some(a+2))
//    Some(3)
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)
        )
      )
  }

  //  List(Some(1),Some(2),Some(3)) => 1 :: sequence(Some(2),Some(3)) => Some(Nil)
  //  Some(Nil) -> Some(2 :: Some(3 :: Nil))
  //

//  /*4.4*/
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
//    case Nil => Some(Nil)
//    case h::t => h.flatMap(hh=>)
//  }
//  /*4.5*/
//  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
//    case Nil => Some(Nil)
//    case h::t =>
//  }
//
//  //traverse이용
//  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
//    case Nil => Some(Nil)
//    case h::t =>
//  }
}

object Test2 {
  def main(args: Array[String]): Unit = {
/*  map2가 뭐하는 함수인가에 대한 설명
    Option안붙은 함수 f를 옵션 붙은 파라미터에
    별다른 조작없이 적용할 수 있게 해주는게 map2
*/
    val f = (a: Int, b: Int) => a + b
    println(Option.map2(Some(1), Some(2))(f));

    val x = 1
    println("===map===")
    println(Some(x).map(x => x + 1))
    println(Some(x).map(a => a + 1)) //a가 되든 x가 되든 상관이 없다. java .map처럼

    println("===getOrElse===")
    println(Some(x).getOrElse(0))
    println(None.getOrElse(0))

    println("===flatMap===")
    println(Some(x).flatMap(x => Some(x+1)))
    println(None.flatMap(x => Some(1)))

    println("===orElse===")
    println(Some(x).orElse(Some(0)))
    println(None.orElse(Some(0)))

    println("===filter===")
    println(Some(x).filter(x => x==1))
    println(Some(x).filter(x => x!=1))
    println(None.filter(x => x==1))

    println("===filter1===")
    println(Some(x).filter1(x => x==1))
    println(Some(x).filter1(x => x!=1))
    println(None.filter1(x => x==1))

    println("===variance===")
    println(Option.variance(Seq(1,2,3,4,5))) //2.5인데 왜..2가 나오지..?ㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋ

    println("===map2===")
    println(Option.map2(Some(1), Some(2))(f))
    println(Option.map2(None, Some(2))(f))
    println(Option.map2(None, None)(f))
  }
}