package fpinscala.monads.soeun

import fpinscala.parsing.answer._
import fpinscala.testing.answer._
import fpinscala.parallelism.answer._
import fpinscala.state.answer._
import fpinscala.parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /*3*/
  def sequence[A](lma: List[M[A]]): M[List[A]] = {
//    val z = List[A]()
    val z = unit(List[A]())
//    val f: (M[A], M[A]) => M[List[A]] = (ma, mb) => map2(ma, mb)(_ :: _)
    lma.foldRight(z)((ma, mb) => map2(ma, mb)(_ :: _))
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = {
    //최종형태가 B
//    val z = unit(List[A]())
    val z = unit(List[B]())
    la.foldRight(z)((ma, mb) => map2(f(ma), mb)(_ :: _))
  }
  /*4*/
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill(n)(ma))
  }
  /*7*/
  //op(op(x,y),z) == op(x, op(y,z))
  // A => F[B]
//  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
//    flatMap(f)(g)
//  }

  /*10*/ //항등볍칙 증명
//  compose(f, unit) == f
//  flatMap(x)(unit) == x

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???

  /*12*/ //Par 챕터 참고
  def join[A](mma: M[M[A]]): M[A] = {
    flatMap(mma)(a => a)
  }

  /*13*/
  def flatMap[A, B](a: M[A])(f: A => M[B]): M[B] = {
    val b = map(a)(f)
    join(b)
  }

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }
  /*1*/
//  val parMonad: Monad[Par] = new Monad[Par] {
//    def unit[A](a: => A): Par[A] = Par.unit(a)
//    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = {
//      ma flatMap f
//    }
//  }

//  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
//
//  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = {
      ma flatMap f
    }
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = {
      ma flatMap f
    }
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]) = {
      ma flatMap f
    }
  }

  def stateMonad[S] = ???

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

/*17*/
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: A) = Id(a)
    override def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] = a flatMap f
  }
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

