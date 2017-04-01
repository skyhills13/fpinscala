package fpinscala.monoids.toby

import fpinscala.monoids.toby.TreeFoldable.foldRight
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  /* Ex 10.1 */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  /* Ex 10.2 */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = None
  }

  /* Ex 10.3 */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A=>A] {
    def op(a1: A=>A, a2: A=>A): A=>A = a1 andThen a2
    def zero: A=>A = a => a
  }

  /* Ex 10.4 ... */

//  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
//  // data type from Part 2.
//  trait Prop {}
//
//  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
//  // data type from Part 2.
//
//  import fpinscala.testing.answer._
//  import Prop._
//  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")
//
  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /* Ex 10.5 */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a,b) => m.op(f(a),b))

  /* Ex 10.6 */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  /* Ex 10.10 */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a2, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l,w,r)) => Part(a + l, w, r)
      case (Part(l,w,r), Stub(b)) => Part(l, w, r + b)
      case (Part(l1,w1,r1), Part(l2,w2,r2)) => Part(l1, w1+ w2 + (if ((l2 + r1).isEmpty) 0 else 1),  r2)
    }
    def zero: WC = Stub("")
  }

  /* Ex 10.11 */
  def count(s: String): Int = ???

  /* Ex 10.16 */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A,B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a1._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  /* Ex 10.17 */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = ???
    def zero: (A) => B = ???
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

object Test {
  def main(args: Array[String]): Unit = {
    import Monoid._

    /* Ex 5.2 */
    println("Ex5.2: Some(1) = " + optionMonoid.op(Some(1), None))
    println("Ex5.2: Some(1) = " + optionMonoid.op(None, Some(1)))

    /* Ex 5.3 */
    val endoA = (a:Int)=>a+1
    val endoB = (a:Int)=>a*2
    val endoC = (a:Int)=>a-5

    println("Ex5.3: 2 = " + endoMonoid.op(endoA, endoMonoid.zero).apply(1))
    println("Ex5.3: 2 = " + endoMonoid.op(endoMonoid.zero, endoA).apply(1))

    println("Ex5.3: " +
          endoMonoid.op(endoMonoid.op(endoA, endoB), endoC).apply(10) + " = " +
          endoMonoid.op(endoA, endoMonoid.op(endoB, endoC)).apply(10))

  }
}

/* Ex 10.12 */
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =  foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  /* Ex 10.15 */
  def toList[A](as: F[A]): List[A] = foldLeft(as)(List[A]())((b,a) => a :: b)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a,b) => mb.op(f(a),b))

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a,b) => mb.op(f(a),b))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

/* Ex 10.13 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l,r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l,r) => foldRight(r)(foldRight(l)(z)(f))(f)
  }
}

/* Ex 10.14 */
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
}



