//package fpinscala.monoids
//
//import fpinscala.parallelism.Nonblocking._
//import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
//import language.higherKinds
//
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
//
object Monoid {
//
//  val stringMonoid = new Monoid[String] {
//    def op(a1: String, a2: String) = a1 + a2
//    val zero = ""
//  }
//
//  def listMonoid[A] = new Monoid[List[A]] {
//    def op(a1: List[A], a2: List[A]) = a1 ++ a2
//    val zero = 1
//    val zero = Nil
//  }
//
//  //IDE에서 자동완성 해줬음!! 어떻게 해준 것인지 자세히 살펴보장
////  val intAddition: Monoid[Int] = new Monoid[Int] {override def op(a1: Int, a2: Int): Int = ???
////
////    override def zero: Int = ???
////  }
///*질문 : 테스트!*/
///*1*/
//  val intAddition: Monoid[Int] = new Monoid[Int] {
//    def op(x: Int, y:Int) = x + y
//    val zero = 0
//  }
//
//  val intMultiplication: Monoid[Int] = new Monoid[Int] {
//    def op(x: Int, y:Int) = x * y
//    val zero = 1
//  }
//
//  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
//    def op(x: Boolean, y: Boolean) = x || y
//    val zero = false
//  }
//
//  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
//    def op(x: Boolean, y: Boolean) = x && y
//    val zero = true
//  }
//
///*2*/
////  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Any] {override def op(a1: Any, a2: Any): Any = ???
////
////    override def zero: Any = ???
////  }
//
//  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
////    def op(x: Option[A], y: Option[A]) = 여기가 뭘까
//    val zero = None
//  }
//
//  /*3*/
//  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
//    def op(x: A => A, y: A => A) = x(y(a))
////    val zero = ....?
//  }
//
//
//  import fpinscala.testing._
//  import Prop._
//  /*4*/
//  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")
//
//  def trimMonoid(s: String): Monoid[String] = sys.error("todo")
//
//  def concatenate[A](as: List[A], m: Monoid[A]): A = {
////    as.foldRight(m.zero)(m.op) 둘다 가능
//    as.foldLeft(m.zero)(m.op)
//  }
//
///*5*/
//  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
////    val f = (x: A, y: A) => m.op(f(x), f(y))
//    val f = (x: A, y: A) => m.op(f(x), y)
//    as.foldRight(m.zero)(f)
//  }
//
///*6*/
//  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
//    sys.error("todo")
//
//  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
//    sys.error("todo")
//
///*7*/
//  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
//     val (l, r) = as.splitAt(as.length/2)
//     m.op(foldMapV(l,m)(f), foldMapV(m,r)(f))
//  }
///*8*/
//  //foldMap 병렬 버전
//
//  def par[A](m: Monoid[A]): Monoid[Par[A]] =
//  sys.error("todo")
//
//  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
//    sys.error("todo")
//
//
///*9*/
//  def ordered(ints: IndexedSeq[Int]): Boolean =
//    sys.error("todo")
//
//  sealed trait WC
//  case class Stub(chars: String) extends WC
//  case class Part(lStub: String, words: Int, rStub: String) extends WC
//
//  /*10*/
//  //꿀잼문제구만 역시 패턴매칭이 잼써 생각해보니 쉽기 때문에 풀 수 있어서 재밌는것 같다 ㅋㅋㅋㅋㅋ
//  val wcMonoid: Monoid[WC] = new Monoid[WC] {
//    def op(x: WC, y: WC) = (x, y) match {
//      case (Stub(a), Stub(b)) => Stub(a + b)
//      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
//      case (Part(l, w, r), Stub(a)) => Part(l, w, r + a)
//      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
//        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
//    }
//    val zero = Stub("")
//  }
///*11*/
//  //그리고 바로 막혔다고 한다
////  def count(s: String): Int = {
////
////  }
//
//
//  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
//    sys.error("todo")
//
//  /*17*/
//  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
////  def op(f: A => B, g: A => B) =
////  val zero: A => B =
//  }
//
//
//  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
//      def zero = Map[K,V]()
//      def op(a: Map[K, V], b: Map[K, V]) =
//        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
//          acc.updated(k, V.op(a.getOrElse(k, V.zero),
//            b.getOrElse(k, V.zero)))
//      }
//  }
///*18*/
////  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
////    foldMapV()
////}
//
///*12*/
//trait Foldable[F[_]] {
//  import Monoid._
//
////  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
////    foldMap(as)(f)(z)
////  }
//
////  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
////    foldMap(as)()
//
//  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
//    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
//
//  def concatenate[A](as: F[A])(m: Monoid[A]): A =
//    foldRight(as)(m.zero)(m.op)
//
//  /*15*/
//  def toList[A](as: F[A]): List[A] =
//    foldRight(as)(List[A]())(_::_)
//}
//
//object ListFoldable extends Foldable[List] {
//  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = {
//    as.foldRight(z)(f)
//  }
//  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = {
//    as.foldLeft(z)(f)
//  }
//  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
//    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b)
//  }
//}
//
//object IndexedSeqFoldable extends Foldable[IndexedSeq] {
//  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
//    as.foldRight(z)(f)
//  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
//    as.foldLeft(z)(f)
//  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
//    foldMapV(as, mb)(f)
//}
//
//object StreamFoldable extends Foldable[Stream] {
//  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
//    as.foldRight(z)(f)
//  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
//    as.foldLeft(z)(f)
//}
///*13*/
//sealed trait Tree[+A]
//case class Leaf[A](value: A) extends Tree[A]
//case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//
//object TreeFoldable extends Foldable[Tree] {
//  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
//    case Leaf(a) => f(a)
//    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
//  }
//  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
//    case Leaf(a) => f(z, a)
//    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
//  }
//
//  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
//    case Leaf(a) => f(a, z)
//    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
//  }
//}
//
////optional을 fold한다는 것
///*14*/
//object OptionFoldable extends Foldable[Option] {
//  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
//    sys.error("todo")
//  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
//    sys.error("todo")
//  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
//}
//
