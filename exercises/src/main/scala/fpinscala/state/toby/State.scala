package fpinscala.state.toby

/**
  * Toby
  */

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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /* Ex 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) (Int.MaxValue, rng2) else (Math.abs(i), rng2)
  }

  /* Ex 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble+1), rng2)
  }

  /* Ex 6.3 */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((d,i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng2)
    ((d1,d2,d3), rng4)
  }

  /* Ex 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight((Nil:List[Int], rng)){
      case ((_, (l, rng))) => {
        val (i, rng2) = nonNegativeInt(rng)
        (i :: l, rng2)
      }
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i -i % 2)

  /* Ex 6.5 */
  def double2(rng: RNG): Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble+1))
  /* 6.2와 동일하게 만들려면 rng 적용까지 해야지 */
  def double2_(rng: RNG): (Double, RNG) = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble+1))(rng)

  /* Ex 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  /* Ex 6.7 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil):Rand[List[A]])((a,l) => map2(a, l)(_ :: _))

  def ints2(count: Int)(rng: RNG): (List[Int],RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

object StateTest {
  def main(args: Array[String]): Unit = {
    import RNG._
    (1 to 10).foreach(x => println("* Ex 6.1: " + nonNegativeInt(Simple(x))._1))
    (1 to 10).foreach(x => println("* Ex 6.2: " + double(Simple(x))._1))
    println("* Ex 6.3: " + intDouble(Simple(1)))
    println("* Ex 6.3: " + doubleInt(Simple(1)))
    println("* Ex 6.3: " + double3(Simple(1)))
    println("* Ex 6.4: " + ints(5)(Simple(1)))

    println("* Ex 6.7: " + sequence(List(unit(1), unit(2), unit(3)))(Simple(1))._1);
  }
}

case class State[S,+A](run: S => (A, S)) {
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
