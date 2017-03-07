package fpinscala.state.soeun

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  //하나의 RNG를 이용해서 A와 새로운 RNG로 전이하는 하나의 프로그램(상태 전이)
  type Rand[+A] = RNG => (A, RNG)

  //TODO 이것..
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    //TODO 얘(rng)는 지금 어떻게 아는거지
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    //TODO 얘(rng)는 지금 어떻게 아는거지
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*1*/
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt
    if (a<0) (-(a + 1), rng2)
    else (a, rng2)
  }

  /*2*/
  def double(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    //정규화
    (a.toDouble / Int.MaxValue.toDouble, rng2)
  }

  /*3*/
  //문제의 의도가 무엇인지 이해가 되지 않아 두가지 종류의 답을 적음
  //1. 같은 랜덤 함수로 인트값 하나 더블값 하나 리턴
  //2. 첫번째에서 나온 랜덤 함수를 그 다음 랜덤함수에 이용
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
//    println(nonNegativeInt(rng)._2, double(rng)._2)
    ((nonNegativeInt(rng)._1, double(rng)._1), nonNegativeInt(rng)._2)
  }

  def intDouble2(rng: RNG): ((Int,Double), RNG) = {
    val (a1, rng2) = nonNegativeInt(rng)
    val (d1, rng3) = double(rng2)
    ((a1, d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
//    println(nonNegativeInt(rng)._2, double(rng)._2)
    ((double(rng)._1, nonNegativeInt(rng)._1), nonNegativeInt(rng)._2)
  }

  def doubleInt2(rng: RNG): ((Double,Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (a1, rng3) = nonNegativeInt(rng2)
    ((d1, a1), rng3)
  }

  //이 문제는 같은 값을 튜플로 받을 이유가 없으므로 의심의 여지가 없이 2번
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (a1, rng2) = double(rng)
    val (a2, rng3) = double(rng2)
    val (a3, rng4) = double(rng3)
    ((a1, a2, a3), rng4)
  }

  /*4*/
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val l = List[Int]()
    def go(count: Int, r: RNG, l: List[Int]) : (List[Int], RNG) = {
      if(count <= 0) (l, r)
      else go(count -1 , nonNegativeInt(r)._2, nonNegativeInt(r)._1 :: l)
    }
    go(count, rng, l)
  }

  /*5*/
  //리턴타입을 처음에는 (Double, RNG)로 했다가 map에서 리턴하는 타입과 맞추기 위해 변경
  def doubleGraceful(rng: RNG): Rand[Double] = {
//    map(nonNegativeInt(rng))(_.toDouble/Int.MaxValue.toDouble)
    map(nonNegativeInt)(_.toDouble/Int.MaxValue.toDouble)
  }

  /*6*/
  //이 문제를 보니 3번은 두번째 방법으로 푸는 게 맞다.
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a1, rng2) = ra(rng)
      val (a2, rng3) = rb(rng2)
      (f(a1, a2), rng3)
    }
  }

  //토탈 카오스가 시작됨
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((_,_))
  }
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  //TODO 다시 풀기
//  foldRight안에 들어가는게 함수의 리턴값이어야해 즉. Rand[List[A]]여야 한다구
  /*7*/
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
////    def go(fs: List[Rand[A]]): Rand[List[A]] = {
////    }
//    fs.foldRight(List[A]())()
//  }

  //이것의 문제는 Int.MaxValue가 n으로 나누어 떨어지지 않을 수도 있어서 난수들이 치우질 수 있다는 것이다.
  //나눗셈의 나머지보다 작은 수들이 좀 더 자주 나타나게 된다.
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    map(nonNegativeInt)(_ % n)
  }

  //형식이 안맞음
//  def nonNegativeLessThan15(n: Int): Rand[Int] = {
//    map(nonNegativeInt){i =>
//      val mod = i % n
//      if(i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)(???)
//    }
//  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if(i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng2)
    }
  }

  /*8*/
  //g() 에서 파라미터 타입힌트 얻어서 i 넣고 ()해서 타입힌트 얻어서 rng2넣고
  //했는데 맞는지도 모르겠고 이해도 안됨 망함 ㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋㅋ
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (i, rng2) = f(rng)
      g(i)(rng2)
    }
  }

  //힌트 : 형식 안맞아서 실패한 예제랑 거의 똑같은 형태
//  def nonNegativeLessThan3(n: Int): Rand[Int] = {
//    flatMap(nonNegativeInt){ i =>
//      val mod = i % n
//      if(i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)
//    }
//  }


  /*9*/
  //힌트 : nonNegativeLessThan과 비슷
//  def mapWithFlatMap[A,B](s: Rand[A])(f:A => B): Rand[B] = {
//    flatMap(s)(a => f(a))
//  }
  //힌트 : Option의 map2를 한번 봐봐
/*
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))
*/
  //힌트 보고 그대로 치환했지만 이해가 되지 않는다
//  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    flatMap(ra)(a => map(rb)( b => f(a, b)))
//  }
}

case class State[S,+A](run: S => (A, S)) {
  /*10*/
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object Test {
  def main(args: Array[String]): Unit = {
    import RNG._
    val initialSeed = 1
    println("6.1 // nonNegativeInt() 384748 - " + nonNegativeInt(Simple(initialSeed)));
    println("==============================")
    println("6.2 // double() 1.79162249052507E-4 - " + double(Simple(initialSeed)));
    println("6.2 // Is double() return value less than 1 - " + (double(Simple(initialSeed))._1 < 1));
    println("==============================")
    println("6.3 // intDouble  - " + intDouble(Simple(initialSeed)))
    println("6.3 // intDouble  - " + intDouble2(Simple(initialSeed)))
    println("6.3 // doubleInt  - " + doubleInt(Simple(initialSeed)))
    println("6.3 // doubleInt  - " + doubleInt2(Simple(initialSeed)))
    println("6.3 // double3  - " + double3(Simple(initialSeed)))
    println("==============================")
    println("6.4 // ints - " + ints(10)(Simple(initialSeed)))
    println("6.4 // ints - " + ints(10)(Simple(initialSeed)))
    println("==============================")
    //TODO 이거 내부 값 보려면 뭔가를 구현해야하나보다
    println("6.5 // doubleGraceful - " + doubleGraceful(Simple(initialSeed)))
    println("==============================")
//    println("6.6 // map2 - " + map2())
//    println("==============================")
//    println("6.7 // sequence List(1, 2, 3) - " + sequence(List(unit(1), unit(2), unit(3)))(Simple(initialSeed))._1)
//    println("==============================")
//    println("6.8 // flatMap - " + flatMap(Simple(initialSeed))(_ + 1))
//    println("6.8 // nonNegativeLessThan - " + nonNegativeLessThan(1))
//    println("==============================")
//    println("6.9 // flatMap - " + flatMap(Simple(initialSeed))(_ + 1))



  }
}
