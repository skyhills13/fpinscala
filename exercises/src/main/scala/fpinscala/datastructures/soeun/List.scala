package fpinscala.datastructures.soeun

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*3.1*/
  //3번째 case에 들어가서 답은 3
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //TODO 이거 main 못찾아온다
  def main(args: Array[String]): Unit = {
    println(x)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*3.2*/
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /*3.3*/
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  /*3.4*/
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) =>
      if(n == 0) l
      else drop(t, n-1)
  }
//  def drop[A](l: List[A], n: Int): List[A] = {
//    def loop(i : Int): List[A] = {
//      if(i + 1 == n) l
//      else
//    }
//    loop(0)
//  }

  /*3.5*/
  //주어진 술어와 부합하는 List의 앞 요소들(prefix)을 제거하는 함수 => n 대신 조건붙은 tail인 셈
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
      //근데 스칼라 너 왜 반대는 안되냐? 후
//      if f(h) dropWhile(t, f)
//      else l
    case Cons(h, t) =>
      if (!f(h)) l
      else dropWhile(t, f)
  }

  /*3.6*/
  //tail처럼 상수 시간으로 구현할 수 없는 이유는? 글쎄 나도 알고 싶다 주르륵 힌트도 없다 잔인하게
  /* 리스트를 구성하는 Cons(h,t)가 헤드 요소 "한 개"와 나머지 부분 전체로 이루어져 있기 때문에*/
//  def init[A](l: List[A]): List[A] = l match {
//    case Nil => Nil
//    case Cons(h, t) =>
//      if t == Cons(_, Nil)
//  }

  /*3.7*/
  //foldRight로 구현된 product가 0.0을 만났을 때 즉시 재귀를 멈추고 0.0을 돌려줄까 왜 그럴까
  //아니라면 왜 아닐까? foldRight를 긴 목록으로 호출했을 때 어떤 평가 단축이 어떤 식으로 일어나는지 고찰하라.
  /*확인하고 싶은데 trace 확인을 어떻게 하는 걸까. 일단 product2의 경우에는 0.0에 대한 case처리를 따로
  * 할 수 없기 때문에 즉시 재귀를 멈추지 않고 재귀가 계속 될 것 같다. 그럴 줄 알았는데 테스트해보니까 시간이 현저하게
  * 적게 든다. 고로 즉시 멈춘다. 어떻게 가능한거지.
  * 힌트를 보니까, Cons(h,t)일 때, h부터 차근차근 평가 단축을 해가므로, 0.0을 만나는 순간 그 뒤는 해 볼 필요가
  * 없어지는 것 같다.*/

  /*3.8*/
  //foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))^10처럼 Nil과 Cons 자체를 foldRight에
  //전달하면 어떤 일이 발생할까? 이로부터, foldRight와 List의 자료 생성자들 사이의 관계에 관해 어떤 점을 알 수 있는가?
  /*뭔 뜻인지 모르겠다*/

  /*3.9*/
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => y + 1)
  }

  /*3.10*/
  //하..힌트도 엄써 ㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋ 자체 힌트
  //테스트는 또 어케 함 => sum이랑 product로 하면 됨 ^6^
//  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
//  l match {
//    case Nil => z
//    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//  }
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /*3.11*/
  def sum3(ns: List[Int]) : Int = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  def product3(ns: List[Double]) : Double = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length3[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, _) => x + 1)
  }

  /*3.12*/
  //띠용
//  def reverse[A](l : List[A]) : List[A] = {
//  }

  /*3.13*/
  //foldLeft를 foldRight를 이용해서 구현할 수 있을까? 그 반대 방향은 어떨까?
  //할 수 있으니까 문제를 냈나본데, 하..

  /*3.14*/
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  /*3.15*/
  //목록들의 목록을 하나의 목록으로 연결하는 함수

  /*3.16*/
//  정수 목록의 각 요소에 1을 더해서 목록을 변환
  def plusOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1, t))
  }

  /*3.17*/
  def convertToString(l : List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
  }

  /*3.18*/
  //이것이 만약 정답이라면 넘나 뿌듯할 것이다. 조금이나마 이해를 한 것이기 때문
  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  /*3.19*/
  def filter[A](as:List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) Cons(h,filter(t)(f))
      else filter(t)(f)
  }

  def deleteOdd(l : List[Int]): List[Int] = {
    filter(l)(x => x % 2 == 0)
  }

  /*3.20*/
//  def flatMap[A,B](as: List[A])(f: A => List[B]):List[B] = {
//    foldRight(as, Nil:List[B])((x, y) => Cons(f(x), y))

//    case Nil => Nil
//    case Cons(h, t) => Cons(f(h), flatMap(t)(f))
//  }

//  /*3.21*/
//  //flatMap이용해서 구현
//  def filter2[A](as:List[A])(f: A => Boolean): List[A] = {
//
//  }

  /*3.22*/
  //목록 두개를 받아서 대응되는 요소들을 더한 값들로 이루어진 새 목록 반환
  def parallelAddition(a1:List[Int], a2:List[Int]): List[Int] = (a1,a2) match {
    case(Nil, _) => a2
    case(_, Nil) => a1
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, parallelAddition(t1,t2))
  }

  /*3.23*/
  def zipWith[A](a1:List[A], a2:List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case(Nil, _) => a2
    case(_, Nil) => a1
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

//  /*3.24*/
//  def hasSubsequence[A](sup:List[A], sub:List[A]): Boolean = {
//
//  }
}

object Temp {
  def main(args: Array[String]): Unit = {
    import List._
    println(x)

    println(tail(Nil))
    println(tail(List(1, 2, 3)))

    println(setHead(List(1,2,3), 5))
    println(setHead(Nil, 1))

    println(drop(List(3,5,2,4,1), 1))
    println(drop(List(3,5,2,4,1), 3))

    println(dropWhile(List(3,5,2,4,1), (x : Int) => x == 1))
    println(dropWhile(List(3,3,2,4,1), (x : Int) => x == 3))

//    println(init(List(3,5,2,4,1)))

    val t0 = System.nanoTime();
    product2(List(1.0,2.0,3.0,4.0,5.0))
    val t1 = System.nanoTime();
    println("elapsed time : " + (t1 - t0))
    val t2 = System.nanoTime();
    product2(List(0.0,23.0,12.0,0.0,5.0))
    val t3 = System.nanoTime();
    println("elapsed time : " + (t3 - t2))

    println(length(List(1,2,3,4)))
    println(length(Nil))

    println(sum3(List(1,2,3,4,5)))
    println(product3(List(1.0,2.0,3.0,4.0,5.0)))
    println(length3(List(1,2,3,4)))
    println(length3(Nil))

    println(append2(List(1,2,3), List(4,5)))
    println(append2(Nil, List(1,2,3)))

    println(plusOne(List(1,2,3)))

//    맞는건지 틀린건지 알 길이..
    println(convertToString(List(1.0, 2.0, 3.0)))

    println(deleteOdd(List(1,2,3,4,5,6)))

    println(parallelAddition(List(1,2,3), List(4,5,6)))
    println(zipWith(List(1,2,3), List(4,5,6))(_ * _))
  }
}

