//package fpinscala.applicative.soeun
//
//import fpinscala.monads.answer.Functor
//import fpinscala.monads.answer.Monad
//import fpinscala.state.answer._
//import State._
//import StateUtil._ // defined at bottom of this file
//import fpinscala.monoids.answer._
//import language.higherKinds
//import language.implicitConversions
//
//trait Applicative[F[_]] extends Functor[F] {
//
//
//  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
//
//  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
//
//  def unit[A](a: => A): F[A]
//
//  def map[A,B](fa: F[A])(f: A => B): F[B] =
//    map2(fa, unit(()))((a, _) => f(a))
//
//  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
//    as.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))
//  }
//
//  /*1*/
//  def sequence[A](fas: List[F[A]]): F[List[A]] = {
//    traverse(fas)(a => a)
//  }
//  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
//    sequence(List.fill(n)(fa))
//  }
//  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = {
//    map2(fa, fb)((_,_))
//  }
//
//  /*2*/
////  def map2_s[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
////
////  }
//
////  def map_s[A,B](fa: F[A])(f: A => B): F[B] = {
////    apply(unit(f))(fa)
////  }
//
//  /*3*/ //2번을 풀어야 풀 수 있는데.
//  def map3[A,B,C,D](fa: F[A],
//                    fb: F[B],
//                    fc: F[C])(f: (A, B, C) => D): F[D] = ???
//
//  def map4[A,B,C,D,E](fa: F[A],
//                      fb: F[B],
//                      fc: F[C],
//                      fd: F[D])(f: (A, B, C, D) => E): F[E] = ???
//
//  /*4*///streamApplicative.sequence의 의미 ===> ?????
//  def sequence[A](a: List[Stream[A]]): Stream[List[A]]
//
//  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???
//  /*8*/
//  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
//
//  }
//
//  /*11*/
//  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = ???
//
//  /*12*///네? 네....
//  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
//}
//
//case class Tree[+A](head: A, tail: List[Tree[A]])
//
//trait Monad[F[_]] extends Applicative[F] {
//  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
//
//  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
//
//  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
//    a => flatMap(f(a))(g)
//
//  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
//    flatMap(mf)(f => map(ma)(a => f(a)))
//}
//
//object Monad {
//  //TODO 이거 왜 빨간줄이지
//  /*5*/
//  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
//    def unit[A](a: => A): Either[E, A] = Right(a)
//    override def flatMap[A,B](ei: Either[E, A])(f: A => Either[E, B]) = ei match {
//      case Right(a) => f(a)
//      case Left(b) => Left(b)
//    }
//  }
//  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
//    def unit[A](a: => A): State[S, A] = State(s => (a, s))
//    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
//      st flatMap f
//  }
//
//  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
//    Monad[({type f[x] = F[N[x]]})#f] = ???
//}
//
//sealed trait Validation[+E, +A]
//
//case class Failure[E](head: E, tail: Vector[E])
//  extends Validation[E, Nothing]
//
//case class Success[A](a: A) extends Validation[Nothing, A]
//
//
//object Applicative {
////TODO 이건 예시인데 왜 빨갛지?
////  val streamApplicative = new Applicative[Stream] {
////
////    def unit[A](a: => A): Stream[A] =
////      Stream.continually(a) // The infinite, constant stream
////
////    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
////                    f: (A,B) => C): Stream[C] =
////      a zip b map f.tupled
////  }
//
//  /*6*/
//  //TODO 왜 빨간줄인지 알아내기
////  hint :Implement `map2` using pattern matching. If both sides are a failure, try to keep the order of failures consistent.
//  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
//    def unit[A](a: => A) = Success(a)
//    override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C) = (fa, fb) match {
//      case (Success(a), Success(b)) => Success(f(a, b))
//      case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta)
//      case (Success(a), Failure(hb, tb)) => Failure(hb, tb)
//      case (Failure(ha, ta), Success(b)) => Failure(ha, ta)
//    }
//  }
//
//  type Const[A, B] = A
//
//  implicit def monoidApplicative[M](M: Monoid[M]) =
//    new Applicative[({ type f[x] = Const[M, x] })#f] {
//      def unit[A](a: => A): M = M.zero
//      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
//    }
//}
//
//trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
//  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
//    sequence(map(fa)(f))
//  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
//    traverse(fma)(ma => ma)
//
//  def map[A,B](fa: F[A])(f: A => B): F[B] = ???
//
//  import Applicative._
//
//  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
//    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
//      as)(f)(monoidApplicative(mb))
//
//  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
//    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)
//
//  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
//    traverseS(fa)((a: A) => (for {
//      s1 <- get[S]
//      (b, s2) = f(a, s1)
//      _  <- set(s2)
//    } yield b)).run(s)
//
//  override def toList[A](fa: F[A]): List[A] =
//    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
//
//  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
//    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
//
//  /*16*/
//  def reverse[A](fa: F[A]): F[A] = {
//    mapAccum(fa, toList(fa).reverse)((a, b) => ((), ))
//  }
//  /*17*/
//  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = {
//    mapAccum(fa, z)((a, b) => ((), f(b, a)))
//  }
//
//  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
//                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???
//
//  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
//}
//
///*13*/
//object Traverse {
//
////  참고 하삼 타입만 따라 가면 된다고 힌트에..
////  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = {
////    as.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))
////  }
//  val listTraverse = new Traverse[List] {
//    override def traverse[G[_]: Applicative, A, B](l: List[A])(f: A => G[B])(g: Applicative[G]): G[List[B]] =
//      l.foldRight(g.unit(List[B]()))((a, b) => g.map2(f(a), b)(_ :: _))
//  }
//
//  val optionTraverse = new Traverse[Option] {
//    override def traverse[G[_]: Applicative,A,B](o: Option[A])(f: A => G[B])(g: Applicative[G]): G[Option[B]] =
//      o match {
//        case Some(a) => g.map(f(a))(Some(_))
//        case None    => g.unit(None)
//      }
//  }
//
//  val treeTraverse = ???
//}
//
//// The `get` and `set` functions on `State` are used above,
//// but aren't in the `exercises` subproject, so we include
//// them here
//object StateUtil {
//
//  def get[S]: State[S, S] =
//    State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] =
//    State(_ => ((), s))
//}
