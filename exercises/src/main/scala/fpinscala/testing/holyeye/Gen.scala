package fpinscala.testing.holyeye

import fpinscala.laziness.holyeye.Stream
import fpinscala.state.holyeye._
import fpinscala.parallelism.holyeye._
import fpinscala.parallelism.holyeye.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.holyeye.SimpleRNG

import language.postfixOps
import language.implicitConversions

//trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]

//Ex 8.2
//  def &&(p: Prop): Prop = {
//    new Prop {
//      def check = Prop.this.check && p.check
//    }
//  }
//}



case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case a => a
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(failedCase, _) => p.tag(failedCase).run(max, n, rng)
        case a => a
      }
  }

  def tag(failedMessage: FailedCase): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(failedCase, successCount) => Falsified(failedMessage + "|" + failedCase, successCount)
        case a => a
      }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  //  type Result = Option[(FailedCase, SuccessCount)]
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1

      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)

//      println("casesPerSize=" + casesPerSize)
//      println("props=" + props)
//      println("prop=" + prop)

      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests: \n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
}


case class Gen[+A](sample: State[RNG, A]) {

  //Ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  //참고
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))


  //Ex 8.6
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    //    val r1 = size.flatMap(s => Gen.unit(List.fill(s)(sample)))
    //    val r2: State[RNG, List[State[RNG, A]]] = r1.sample
    size.flatMap(s => Gen.listOfN(s, this))
  }

  //Ex 8.10
  def unsized: SGen[A] = {
    SGen(forSize => this)
  }

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

}

object Gen {
  //Ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(a => (a % (stopExclusive - start)) + start))
  }

  //Ex 8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State(RNG.unit(a)))
  }

  //Ex 8.5
  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(a => a % 2 == 0))
  }

  //Ex 8.5 //정답 참고
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val l: List[State[RNG, A]] = List.fill(n)(g.sample)
    val sequence: State[RNG, List[A]] = State.sequence(l)
    val result: Gen[List[A]] = Gen(sequence)
    result
  }

  //Ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(if (_) g1 else g2)
  }

  //Ex8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(Gen.unit(n)))
  }
  //Ex 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.unit(List()))
  }


}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}


object GenTest {


  def main(args: Array[String]): Unit = {
    val g = SimpleRNG(System.currentTimeMillis())
    val g1 = SimpleRNG(1)
    val g2 = SimpleRNG(2)
    val maxSize = 10
    println("ex8.4: " + choose(2, 10).sample.run(g)._1)
    println("ex8.5 unit: " + unit(10).sample.run(g)._1)
    println("ex8.5 boolean: " + boolean.sample.run(g)._1)
    println("ex8.5 listOfN: " + listOfN(3, choose(2, 10)).sample.run(g))
    val result: Gen[List[Int]] = listOfN(3, choose(2, 10))
    println(result)
    println(result.sample)
    println(result.sample.run)
    println(result.sample.run(g))

    println("ex8.6: flatMap: " + unit(10).flatMap(a => unit(a * 10)).sample.run(g))
    println("ex8.6: listOfN: " + choose(1, 100).listOfN(unit(5)).sample.run(g))
    println("ex8.7: union: " + Gen.union(unit(10), unit(20)).sample.run(g))
    println("ex8.7: union: " + Gen.union(unit(10), unit(20)).sample.run(g2))
    println("forAll: " + forAll(Gen.choose(1, 10))(a => a < 5).run(maxSize, 5, g))
    println("forAll: " + (forAll(Gen.unit(4))(a => a < 5) && forAll(Gen.unit(10))(_ == 9)).run(maxSize, 5, g))
    println("ex8.9: " + (forAll(Gen.unit(4))(a => a < 5) && forAll(Gen.unit(10))(_ == 9)).run(maxSize, 5, g))
    println("ex8.9: " + (forAll(Gen.unit(4))(a => a < 5).tag("test1") || forAll(Gen.unit(10))(_ == 9).tag("test2")).run(maxSize, 5, g))
    println("ex8.9: " + (forAll(Gen.unit(10))(a => a < 5).tag("test1") || forAll(Gen.unit(10))(_ == 9).tag("test2")).run(maxSize, 5, g))
    println("ex8.9: " + (forAll(Gen.unit(2))(a => a < 5).tag("test1") && forAll(Gen.unit(10))(_ == 9).tag("test2")).run(maxSize, 5, g))
    println("ex8.10: " + Gen.unit(10).unsized.forSize(3).sample.run(g))
    println("ex8.12: " + Gen.listOf(Gen.choose(1, 10)).forSize(3).sample.run(g))
    println("ex8.12 test: " + Gen.listOf(Gen.choose(1, 10)))
    println("ex8.12 test: " + Gen.listOf(Gen.choose(1, 10)).forSize(3))
    println("ex8.12 test: " + Gen.listOf(Gen.choose(1, 10)).forSize(3).sample)
    println("ex8.12 test: " + Gen.listOf(Gen.choose(1, 10)).forSize(3).sample.run(g))
    println("SGen: " + forAll(SGen(a => Gen.choose(1, 9)))(a => a < 2).run(100, 5, g))

    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf1(smallInt)) { ns =>
      if (ns.isEmpty) true
      else {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }
    println("maxProp: " + maxProp.run(100, 4, g))

    run(maxProp, 10, 10)

    //ex 8.14
    val l = Gen.choose(1, 10)
    val sortedProp = forAll(Gen.listOf(l)) { ns =>
      if (ns.isEmpty) true
      else {
        (ns.sorted.min == ns.sorted.head) &&
          (ns.sorted.max == ns.sorted.apply(ns.size - 1))
      }
    }

    println("====== ex 8.14 ======")
    run(sortedProp)

    println("gogo: " + (Par.map(Par.unit(1))(_ + 1) == Par.unit(2)))

    val ES: ExecutorService = Executors.newCachedThreadPool
    val p1 = forAll(Gen.unit(Par.unit(1)))(i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
    )

    println("p1" +run(p1))
    run(check(true))

    run(check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    })

    def equals[A](p: Par[A], p2: Par[A]): Par[Boolean] =
      Par.map2(p, p2)(_ == _)

    run(check {
      equals(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    })


    val S = Gen.union(choose(1,2), unit(3))

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S.map2(g)((_,_))) {case (s,a) => f(a)(ES).get}

    def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) {case (s,a) => f(a)(ES).get}

    def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) {case s ** a => f(a)(ES).get}

    val pint = Gen.choose(0, 10) map (Par.unit(_))
    val p4 = forAllPar(pint)(n => equals(Par.map(n)(y => y), n))
    run(p4)

  }

}
