package fpinscala.monads.holyeye

import fpinscala.testing.answer.Gen

/**
  * Created by younghankim on 2017. 3. 29..
  */

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(a => a._1), map(fab)(b => b._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: (A) => B): List[B] = as.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  //ex 11.3(정답 참고)
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((fa, b) => map2(fa, b)(_ :: _))
//    lma.flatMap(a => map(a)(b:A => b))
//    unit(lma.flatMap(a => map(a)(b => b)))
//    unit(lma.flatMap(a => map(a)(b => b)))

//  def sequence[A](lma: List[Option[A]]): Option[List[A]] =
//    Some(lma.flatMap((a: Option[A]) => a.map((b: A) => b)))
//    Some(lma.flatMap(a => a.map(b => b))

//  def traverse[A,B](la: List[A])(f: A => Option[B]): Option[List[B]] =
  //ex 11.3(정답참고)
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a,b) => map2(f(a), b)(_ :: _))

  //ex 11.4
  def replicateM[A](n: Int, ma:F[A]): F[List[A]] = {
    val filled: List[F[A]] = List.fill(n)(ma)
    sequence(filled)
  }

  //ex 11.7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  //ex 11.12
  def join[A](mna: F[F[A]]): F[A] = {
//    flatMap(mna)(a => flatMap(a)(_ => a))
    flatMap(mna)(a => a)
  }

  //ex 11.13 (정답참고)
  def flatMap2[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    join(map(fa)(f))
  }

}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  //Ex 11.1
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)
  }
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
  }
}

//ex 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object MonadTest {
  def main(args: Array[String]): Unit = {
    println(Functor.listFunctor.map(List(1, 2, 3))(_ + 10))
    println(Functor.listFunctor.distribute(List((1, 2), (3, 4), (5, 6))))
    println(Functor.listFunctor.codistribute(Left(List(1, 2, 3))))

    println(Right(List(1, 2), List(2, 3)))

    println(Monad.optionMonad.unit(10))
    println(Monad.optionMonad.flatMap(Some(10))(a => Some(a + 20)))

    println(Monad.listMonad.unit(1))
    println(Monad.listMonad.flatMap(List(1,2,3))(a => List(a * 10)))

    println(Monad.optionMonad.sequence(List(Some(1), Some(2))))

//    println(Monad.listMonad.compose((a) => List(a), (b) => List(b)))

    println(Id("hi").flatMap(a => Id("go").flatMap(b => Id(a + b))))

    val result = for {
      a <- Id("hi")
      b <- Id("go")
    } yield(a + b)
    println(result)
  }
}