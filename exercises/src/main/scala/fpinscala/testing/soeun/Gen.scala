package fpinscala.testing.soeun

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

/*8.1*/
/*sum의 속성 고안*/
/*=========================================*/
//뒤집어서 합산해도 원래의 목록의 합산과 같은 결과가 나와야 한다.
//목록의 모든 요소가 값이 같다면 결과는 (요소 하나의 값 * 요소의 갯수)가 되어야 한다
//결과는 목록의 어떤(any) 요소보다도 크거나 같아야 한다.

/*8.2*/
/*=========================================*/
/*최댓값을 찾는 함수를 명시하는 속성*/
//최댓값은 목록의 어떤 요소보다도 크거나 같아야 한다.

/*8.3*/
//무엇을 하라는 건지 잘 모르겠다 ㅠㅠㅠㅠㅠ흙흙

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

//  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis()): Unit =
//    p.run(maxSize, testCases, rng) match {
//      case Falsified(msg, n) =>
//        println(s"! Falsified after $n passed tests: \n $msg") //TODO msg명세
//      case Passed =>
//        println(s"+ Ok, passed $testCases tests")
//    }
}

case class Gen[A](sample: State[RNG, A]) {

  /*8.6*/
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = {
    //형식만 맞추자 형식만!!!제발
//    Gen(sample.flatMap(a => f(a)))
    ???
  }

//  def listOfN(size: Gen[Int]): Gen[List[A]] = {
//
//  }

  /*8.7*/
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    ???
  }

  /*8.8*/
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???

  /*8.10*/
  //Gen의 메서드로 추가하래
  def unsized: SGen[A] = {
    SGen(a => this)
  }

  /*8.11*/
  //SGen에 대해서 그 밖에 필요한 메서드

  /*8.12*/
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
//    SGen(listOf(g))
    ???
  }
}

object Gen {

  /*8.4*/
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    //RNG를 이용하면 되겠는데 구현한 것중엔 start도 포함하는게 없으므로 새로 구현해야함
    //RNG.nonNegativeLessThan()
    ???
  }

  /*8.5*/
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    ???
  }

//  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
//    Gen(State.sequence(List.fill(n)(a)))
//  }

}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
}

/*
sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
  //이거는 아마도 prop안에 들어가겠지 Gen이 아니라
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

*/

//def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
//  forAll(g(_))(f)
//
//def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
//  (max,n,rng) =>
//    val casesPerSize = (n + (max - 1)) / max
//    val props: Stream[Prop] =
//      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f)) //각 크기마다 하나의 속성을 만든다. 단 전체 속성의 갯수가 n을 넘지는 않게 한다.
//    val prop: Prop =
//      props.map(p => Prop { (max, _, rng) =>
//        p.run(max, casesPerSize, rng)
//      }).toList.reduce(_ && _) //모든 속성을 하나의 속성으로 결합.
//    prop.run(max,n,rng)
//}


case class SGen[+A](forSize: Int => Gen[A]){
}

trait SGen[+A] {

}

