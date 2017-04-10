package fpinscala.applicative.holyeye

import fpinscala.monads.holyeye.{Functor, Monad}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: (A) => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  //ex 12.1
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(a => a)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  //ex 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a, b) => a(b))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  //정답참고
  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  //  (A) => ((B) => C)

  //ex 12.3 정답참고
  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
  //    apply(apply(map(fa)(f.curried)))
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  //ex 12.8 ㅠㅠ
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = ???

  //ex 12.12 ㅠㅠ
  //  def sequenceMap[K,V](ofa: Map[K, F[V]]): F[Map[K,V]] =
  //    ofa.foldRight(Map.empty[K,V])((op => op match { case (acc, (k, fv)) =>
  //      map2(acc, fv)((m,v) => m + (k -> v))
  //    })


}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

trait Traverse[F[_]] {


  def traverse[G[_] : Applicative, A, B](as: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  //  sequence(map(as)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
}

object Traverse {
  //ex 12.13 정답참고
  val listTraverse = new Traverse[List] {
    def traverse[G[_] : Applicative, A, B](as: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a: A, b: G[List[B]]) => G.map2(f(a), b)(_ :: _))
  }

  //ex 12.13 정답참고
  val optionTraverse = new Traverse[Option] {
    def traverse[G[_] : Applicative, A, B](as: Option[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      as match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = ???
}


object ApplicativeMain {

  //ex 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {

      def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa match {
        case Left(a) => Left(a)
        case Right(a) => f(a)
      }

      def unit[A](a: => A): Either[E, A] = Right(a)
    }

  //ex 12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {

      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(a), Failure(h, tail)) => Failure(h, tail)
        case (Failure(h, tail), Success(a)) => Failure(h, tail)
        case (Failure(h, tail), Failure(h2, tail2)) => Failure(h, Vector(h2) ++ tail ++ tail2)
      }

      def unit[A](a: => A): Validation[E, A] = Success(a)
    }

  //ex 12.13

  def main(args: Array[String]): Unit = {
    println("start")

    val result = eitherMonad.unit(10)
    println(result)
  }
}
