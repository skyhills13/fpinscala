package fpinscala.laziness.soeun

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  /*5.1
  * Stream을 List로 변환하되 평가를 강제해서 REPL로 목록의 요소들을 볼 수 있게 하는 함수를 작성하라.
  * */
  def toList: List[A] = this match {
    case Cons(h,t) => h()::t().toList
    case _ => List()
  }

  /*5.2
  * 처음 n개의 요소를 돌려주는 함수
  * */
  def take(n: Int): Stream[A] = this match {
    //case 앞에 if를 거는게 좋겠지. 케이스 안걸린거는 그냥 empty 주면 되니까
    case Cons(h, t) =>
      if(n == 1) cons(h(), empty)
      else if(n > 1) cons(h(), t().take(n-1))
      else empty
    case empty => empty
  }
  /*처음 n개의 요소를 건너뛴 스트림을 돌려주는 함수*/
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) =>
      if(n == 0) this
      else if(n == 1) t()
      else t().drop(n-1)
    case empty => empty
  }

  /*5.3
  * 주어진 술어를 만족하는 선행요소를 모두 돌려주는 함수
  * */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      //if(p(h)) 이렇게하면 type mismatch에라가 난다! 주의해야한다
      if (p(h())) cons(h(), t().takeWhile(p))
      else t().takeWhile(p)
    case empty => empty
  }

  /*5.4
  * 모든 요소가 주어진 술어를 만족하는지. 만족하지 않으면 순회 즉시 마쳐야
  * */
  //TODO foldRight로 다시 해라
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) =>
      if(p(h())) t().forAll(p)
      else false
    case _ => true
  }

  /*5.5
  * foldRight로 takeWhile구현
  * */
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if(p(h)) cons(h,t) else empty)
//    foldRight(empty)((h, t) => if(p(h)) cons(h(),t()) else empty)
  }
  /*5.6
  * foldRight로 구현
  * */
  def headOption: Option[A] = {
    foldRight(None:Option[A])((h,t) => Some(h))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](p: A => B): Stream[B] = {
    foldRight(empty[B])((h,t) => cons(p(h), t))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) => if(p(h)) cons(h, t) else t)
  }

//  def append(a: A): Stream[A] = {
//        foldRight(a)((h,t) => cons(h, t))
//  }

//  def flatMap[B](p: A => Stream[B]): Stream[B] = {
//    foldRight(empty[B])((h,t)=> cons(p(h), t)) //이 상태는 지금 Stream[Stream[B]]
//  }

  /*5.14
  *
  * */
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
  //s의 길이 확인해서, 길이만큼 확인
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /*5.8
  * ones일반화. 주어진 값의 무한stream을 돌려주는 함수.
  * */
  def constant[A](a :A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  /*5.9
  * n에서 시작해서 n + 1, n + 2 등으로 이어지는 무한 정수 스트림을 생성하는 함수
  * */
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  /*5.10
  * 무한 피보나치수 0,1,1,2,3,5,8,... 무한 스트림생성하는 함수
  * */
  def fibs(): Stream[Int] = {
    def go(a: Int, b:Int) : Stream[Int] = {
      cons(a, go(b, a+b))
    }
    go(0, 1)
  }

  /*5.11
  * 초기 상태 하나, 다음 상태 및 다음 값을 산출하는 함수 하나 받아서 일반화된 스트림을 구축하는 함수
  * */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((h, t)) => cons(h, unfold(t)(f))
  }
  /*5.12
  * unfold로 fibs, from, constant, ones
  * */
  def fibs2(): Stream[Int] = {
    unfold((0, 1)){case (a, b) => Some(a, (b, a + b))}
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n+1))
  }

//  def constant2[A](a : A): Stream[A] = {
//    unfold(a)(a => Some(cons(a, a)))
//  }
//  def ones2(): Stream[Int] = {
//    unfold(1)(1 => Some(cons(1,1)))
//  }
  /*5.13
  * unfold로 map, take, takeWhile, zipWith, zipAll
  * */

  /*5.15*/
  /*5.16*/
}

object Test {
  def main(args: Array[String]): Unit = {
    /*3.1*/
    val x = Stream(1,2,3,4,5);
    val y = Stream();
    println("5.2 // take() - " + x.take(0).toList);
    println("5.2 // take() - " + x.take(1).toList);
    println("5.2 // drop() - " + x.drop(0).toList);
    println("5.2 // drop() - " + x.drop(1).toList);
    println("5.2 // drop() - " + x.drop(2).toList);
    println("============================")
    println("5.3 // takeWhile() - " + x.takeWhile(_ % 2 == 0).toList);
    println("============================")
    println("5.4 // forAll() false - " + x.forAll(_ % 2 == 0));
    println("5.4 // forAll() true - " + x.forAll(_ + 1 > 0));
    println("============================")
    println("5.5 // takeWhile2() false - " + x.takeWhile2(_ % 2 == 0).toList);
    println("5.5 // takeWhile2() true - " + x.takeWhile2(_ + 1 > 0).toList);
    println("============================")
    //TODO 이거 왜 Some(1)이 아니지
    println("5.6 // headOption() - " + x.headOption.toList);
    println("5.6 // headOption() none - " + y.headOption.toList);
    println("============================")
    println("5.7 // map() - " + x.map(_ * 2).toList);
    println("5.7 // filter()  - " + x.filter(_ % 2 == 0).toList);
//    println("5.7 // append()  - " + x.append(10).toList);
//    println("5.7 // flatMap()  - " + x.flatMap(_ % 2 == 0).toList);
    println("============================")
    println("5.8 // constant() - expected : 10 10 " + Stream.constant(10).take(2).toList);
    println("============================")
    println("5.9 // from() - expected : 10 11 12 13 " + Stream.from(10).take(4).toList);
    println("============================")
    println("5.10 // fibs() - expected : 0 1 1 2 3 5 8 " + Stream.fibs().take(7).toList);
    println("============================")
//    println("5.11 // unfold() - expected : 0 1 1 2 3 5 8 " + Stream.fibs().take(7).toList);
  }
}