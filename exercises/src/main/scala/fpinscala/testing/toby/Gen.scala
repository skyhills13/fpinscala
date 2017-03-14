package fpinscala.testing.toby

import fpinscala.laziness.answer.Stream
import fpinscala.state._
import fpinscala.testing.toby.Gen._
import fpinscala.testing.toby.Prop._



/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/* Ex 8.3 */
//trait Prop {
//  def check: Boolean
//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = Prop.this.check && p.check
//  }
//}


//trait Prop {
//  //  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//
//}

//case class Prop(run: (TestCases, RNG) => Result) {
case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  /* Ex 8.9 - 이 구현은 Listing 8.4 이후에 class Prop 변화 때문에 사용할 수 없다.*/
//  def &&(p: Prop): Prop = Prop {
//    (n,rng) => run(n,rng) match {
//      case Passed if (p.run(n, rng) == Passed) => Passed
//      case f => f
//    }
//  }
//
//  def ||(p: Prop): Prop = Prop {
//    (n,rng) => run(n,rng) match {
//      case Falsified(fc, sc) if (p.run(n, rng) != Passed) => Falsified(fc, sc)
//      case _ => Passed
//    }
//  }

  def &&(p: Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed if (p.run(max, n, rng) == Passed) => Passed
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(fc, sc) if (p.run(max, n, rng) != Passed) => Falsified(fc, sc)
      case _ => Passed
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* 아래 초기 구현은 Listing 8.4 에서 바뀐 case class Prop 때문에 에러가 난다. 이 때문에 위해 object Prop에 파라미터 2개짜리 apply 메소드를 추가해줘야 한다. */
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  /* g(_) 때문에 SGen의 apply가 필요하다. */
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  /* Listing 8.5 */
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}

case class Gen[+A](sample: State[RNG,A]) {
  /* Ex 8.6 */
  /* A가 이중 타입으로 감싸있는 경우 처리하는 방법 */
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => this.map(a => List.fill(s)(a)))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def unsized: SGen[A] = SGen(size => this)


}

object Test {
  def main(args: Array[String]): Unit = {
//    val x: SGen[SuccessCount] = Gen(State.unit(1)).unsized

  }
}

object Gen {
  /* Ex 8.5 */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

  /* Ex 8.4 */
  // 새로운 함수만 조합하는 방법으로 해야.
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    Gen(State.unit(RNG.nonNegativeLessThan(stopExclusive-start)(Simple(1))._1 + start))
    Gen(State(RNG.nonNegativeLessThan(stopExclusive-start)).map(a => a + start))

  /* Ex 8.7 */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  /* Ex 8.8 */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) =
    Gen(State(RNG.double).map(d => if (d * (g1._2 + g2._2) < g1._2) g1 else g2))

  /* Ex 8.13 */
  // The standard library’s implementation of max crashes when given the empty list. We need to fix our property to take this into account.
  // 문제 설명을 좀 제대로 해야...
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  /* Ex 8.14 */
  val sortedProp = ??? // forAll(listOf(smallInt)) { ns =>

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  /*

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      forSize(n) flatMap { f(_).forSize(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
  */

  // Ex 8.12 ...
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

}

/* Ex 8.1 */
// List가 1개면 원소와 같은 값

/* Ex 8.2 */
// List원소가 1개면 그 원소가 max
// List원소가 모두 같은 값이면 그게 max
