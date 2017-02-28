package fpinscala.state.holyeye

import fpinscala.state.holyeye.RNG._

/**
  * Created by younghankim on 2017. 2. 25..
  */
trait RNG {
  def nextInt: (Int, RNG)
}



object RNG {


  /*
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      //0~Int.MaxValue 이하의 난수 발생기를 찾는다.
      def go(rng: RNG): (Int, RNG) = {
        val (v, newRng) = rng.nextInt
        if (v < 0) go(newRng)
        else (v, rng)
      }
      go(rng)
    }
  */

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, newRng) = rng.nextInt
    (Math.abs(v), newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, newRng) = nonNegativeInt(rng)
    ((v.toDouble / (Int.MaxValue.toDouble + 1)), newRng)
  }

  def double2(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(a => (a.toDouble / (Int.MaxValue.toDouble + 1)))(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (i, newRng) = nonNegativeInt(rng)
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, _) = double(r2)
    ((d1, d2, d3), newRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def go(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (i, r1) = rng.nextInt
        val (l, r2) = go(count - 1)(r1)
        ((i :: l), r2)
      } else {
        (Nil, rng)
      }
    }

    go(count)(rng)
  }

//  type Rand[+A] = RNG => (A, RNG)
  type Rand[+A] = State[RNG, A]
  type State[S, +A] = S => (A,S)

  val int: Rand[Int] = (rng1 => rng1.nextInt) //동작(상태는 없다.)

  //이름이 unit 이라니 설마 모나드?
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)


//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def map[S, A, B](a: S => (A,S))(f: A => B): S => (B,S) =
    rng => {
      val (v, rng2) = a(rng)
      (f(v), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  //    map2(ra, rb)((_,_))
    map2(ra, rb)((a, b) => (a, b))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
    * List(unit(1), unit(2), unit(3))
    * unit(1) :: unit(2) :: unit(3) :: unit(Nil)
    * f(unit(1), (f(unit(2), (f(unit(3), unit(Nil)))) f = map2(a,b)
    * f(unit(1), (f(unit(2), :: unit(List(3)))
    * f(unit(1), :: unit(List(2,3)))
    * unit(List(1,2,3))
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //    fs.foldRight(unit(Nil:List[A]))((a, b) => map2(a,b)((a,b) => b)) //못품
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)((a, b) => a :: b)) //정답확인
  }

  //정답 확인
  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

/*
  def nonNegativeLessThen(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThen(n)(rng2)
  }
*/

  def nonNegativeLessThen(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >=  0) unit(mod) else nonNegativeLessThen(n)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
//    flatMap(ra)(a => map(rb)(b => f(a,b))) //정답확인
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThen(6))(_ + 1)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


object simpleRNGTest {
  def main(arg: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println("rng1" + rng.nextInt)
    println("rng1" + rng.nextInt)

    println("rng2" + rng.nextInt._2.nextInt)
    println("rng2" + rng.nextInt._2.nextInt)

    println("ex6.1: " + SimpleRNG(1).nextInt)
    println("ex6.1: " + SimpleRNG(-1).nextInt)
    println("ex6.1: " + RNG.nonNegativeInt(SimpleRNG(1)))
    println("ex6.1: " + RNG.nonNegativeInt(SimpleRNG(-1)))

    println("ex6.2: " + RNG.double(SimpleRNG(1)))
    println("ex6.2: " + RNG.double(SimpleRNG(2)))

    println("ex6.3: " + RNG.intDouble(SimpleRNG(1)))
    println("ex6.3: " + RNG.doubleInt(SimpleRNG(1)))
    println("ex6.3: " + RNG.double3(SimpleRNG(1)))

    println("ex6.4: " + RNG.ints(5)(SimpleRNG(1)))
    println("ex6.4: " + RNG.ints(5)(SimpleRNG(1))._2.nextInt)


    println("int: " + int(SimpleRNG(1)))
    println("unit: " + unit(1)(SimpleRNG(1)))
    println("unit: " + unit(2)(SimpleRNG(1)))
    println("nonNegativeEven" + RNG.nonNegativeEven(SimpleRNG(2)))

    println("ex6.5: " + RNG.double2(SimpleRNG(1)))
    println("ex6.5: " + RNG.double2(SimpleRNG(2)))

    println("ex6.6: randIntDouble: " + randIntDouble(SimpleRNG(1)))
    println("ex6.6: randDoubleInt: " + randDoubleInt(SimpleRNG(1)))

    println("ex6.7: " + sequence(List(unit(1), unit(2), unit(3)))(SimpleRNG(1))._1) //List(1, 2, 3)
    println("ex6.7: " + ints2(5)(SimpleRNG(1))) //List(1, 2, 3)

    println("nonNegativeLessThen: " + nonNegativeLessThen(10)(SimpleRNG(1)))
    println("map: " + map(nonNegativeInt)(i => i - i % 2)(SimpleRNG(1)))
    println("flatMap: " + flatMap(nonNegativeInt)(i => unit(i - i % 2))(SimpleRNG(1)))
    println("mapViaFlatMap: " + mapViaFlatMap(nonNegativeInt)(i => i - i % 2)(SimpleRNG(1)))

    println("map2: " + map2(unit(1), unit(2))((a,b) => a + b)(SimpleRNG(1)))
    println("map2: " + map2ViaFlatMap(unit(1), unit(2))((a,b) => a + b)(SimpleRNG(1)))

    println("rollDie:" + rollDie(SimpleRNG(1)))
    println("rollDie:" + rollDie(SimpleRNG(2)))
    println("rollDie:" + rollDie(SimpleRNG(3)))
    println("rollDie:" + rollDie(SimpleRNG(4)))
    println("rollDie:" + rollDie(SimpleRNG(5)))
    println("rollDie:" + rollDie(SimpleRNG(6)))
    unit(1)(SimpleRNG(1))


  }
}
