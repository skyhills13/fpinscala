package fpinscala.monoids.holyeye

/**
  * Created by younghankim on 2017. 3. 22..
  */
trait Monoid[A] {
  def op(a: A, b: A): A

  def zero: A
}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String): String = a + b

    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]): List[A] = a ++ b

    def zero: List[A] = Nil
  }

  //ex 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b

    def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b

    def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b

    def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a && b

    def zero: Boolean = true
  }

  //ex 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    def zero = None
  }

  //ex 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a: (A) => A, b: (A) => A): (A) => A = a.andThen(b)

    def zero: (A) => A = (A => A)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  //ex 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    val result = as.foldRight(m.zero)((a, b) => m.op(f(a), b))
    result
  }

  //ex 10.6
  //  def foldRight2[A,B](as: List[A], m: Monoid[B])(z: B)(f: (A,B) => B): B = ???

  //ex 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    if (v.length == 0) {
      m.zero
    } else if (v.length == 1) {
      f(v(0))
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      val result: B = m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      println("result: " + result)
      result
    }
  }

  /*
    "abcd"
    ab|cb
    S(a)|S(b)|S(c)|S(d)
    S(ab)|S(cd)
    S(abcd)

  */
  //ex 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC = a match {
      case Stub(aw) => b match {
        case Stub(bw) => Stub(aw + bw)
        case Part(bl, bc, br) => Part(aw + bl, bc, br)
      }
      case Part(al, ac, ar) => b match {
        case Stub(bw) => Part(al, ac, ar + bw)
        case Part(bl, bc, br) => Part(al, ac + bc + (if (ar.isEmpty && bl.isEmpty) 0 else 1), br)
      }
    }

    def zero: WC = Stub("")
  }

  //ex 10.11 정답 참고
  def wordCount(word: String): Int = {

    def f(a: Char): WC = {
      if (a == ' ')
        Part("", 0, "")
      else
        Stub(a.toString)
    }

    def counter(c: String) = {
      if (c.length > 0) 1 else 0
    }

    val result: WC = foldMapV(word, wcMonoid)(f)

    result match {
      case Stub(c) => counter(c)
      case Part(l, c, r) => counter(l) + c + counter(r)
    }
  }


  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  //ex 10.12
  object List2 extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  object IndexedSeq2 extends Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  object Stream2 extends Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  //ex 10.13 todo
  //  object Tree2 extends Foldable[Tree] {
  //    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
  //      Tree.fold(as)((a) => 1)(f)
  //    }
  //
  //    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???
  //  }

  //ex 10.14
  object FoldableOption extends Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(v) => f(v, z)
    }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(v) => f(z, v)
    }
  }

  def main(args: Array[String]): Unit = {
    println("stringMonoid: " + stringMonoid.op("a", "b"))
    println("stringMonoid: " + stringMonoid.op(stringMonoid.op("0", "a"), "b"))
    println("stringMonoid: " + stringMonoid.op("0", stringMonoid.op("a", "b")))
    println("stringMonoid: " + stringMonoid.op("a", stringMonoid.zero))

    implicit def convertStringToList(s: String) = List(s)

    println("listMonoid: " + listMonoid.op("a", "b"))
    println("listMonoid: " + listMonoid.op(listMonoid.op("0", "a"), "b"))
    println("listMonoid: " + listMonoid.op("0", listMonoid.op("a", "b")))
    println("listMonoid: " + listMonoid.op("a", listMonoid.zero))

    println("ex10.1 intAddition: " + intAddition.op(1, 2))
    println("ex10.1 intAddition: " + intAddition.op(intAddition.op(1, 2), 3))
    println("ex10.1 intAddition: " + intAddition.op(1, intAddition.op(2, 3)))
    println("ex10.1 intAddition: " + intAddition.op(1, intAddition.zero))

    implicit def convertStringToOption(s: String) = Some(s)

    println("ex 10.2 optionMonoid: " + optionMonoid.op("a", "b"))
    println("ex 10.2 optionMonoid: " + optionMonoid.op(optionMonoid.op("0", "a"), "b"))
    println("ex 10.2 optionMonoid: " + optionMonoid.op("0", optionMonoid.op("a", "b")))
    println("ex 10.2 optionMonoid: " + optionMonoid.op("a", optionMonoid.zero))
    println("ex 10.2 optionMonoid: " + optionMonoid.op(optionMonoid.zero, "a"))

    def fun1 = (a: String) => a + "1"

    def fun2 = (a: String) => a + "2"

    def fun3 = (a: String) => a + "3"

    val funOp1 = endoMonoid.op(fun1, fun2)
    val funOp2 = endoMonoid.op(fun2, fun1)
    println("ex 10.3 endoMonoid: " + endoMonoid.op(fun1, fun2)("start"));
    println("ex 10.3 endoMonoid: " + endoMonoid.op(fun1, endoMonoid.op(fun2, fun3))("start"));
    println("ex 10.3 endoMonoid: " + endoMonoid.op(endoMonoid.op(fun1, fun2), fun3)("start"));
    println("ex 10.3 endoMonoid: " + endoMonoid.op(fun1, endoMonoid.zero)("start"));
    println("ex 10.3 endoMonoid: " + endoMonoid.op(endoMonoid.zero, fun1)("start"));

    val words = List("A", "BB", "CCC")
    println("foldRight: " + words.foldRight(stringMonoid.zero)(stringMonoid.op))
    println("foldLeft: " + words.foldRight(stringMonoid.zero)(stringMonoid.op))
    println("concatenate: " + concatenate(words, stringMonoid))

    println("ex 10.5 foldMap: " + foldMap(words, intAddition)(a => a.length))

    val stringList = List("lorem", "ipsum", "dolor", "sit")
    println("foldLeft: " + stringList.foldLeft("")(_ + _))
    println("ex 10.7 foldMapV: " + foldMapV(stringList.toIndexedSeq, stringMonoid)(a => a))
    println("ex 10.7 foldMapV: " + foldMapV(List().toIndexedSeq, stringMonoid)(a => a))

    println("ex 10.10 wcMonoid: " + wcMonoid.zero)
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Stub("abc"), Stub("b")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Stub("a"), wcMonoid.op(Stub("b"), Stub("c"))))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(wcMonoid.op(Stub("a"), Stub("b")), Stub("c")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Stub("a"), wcMonoid.zero))

    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l", 0, "r"), Stub("b")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, "r1"), Part("l2", 3, "r2")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, "r1"), wcMonoid.op(Part("l2", 3, "r2"), Part("l3", 4, "r3"))))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(wcMonoid.op(Part("l1", 2, "r1"), Part("l2", 3, "r2")), Part("l3", 4, "r3")))

    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, "r1"), Part("l2", 3, "r2")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, "r1"), Part("", 3, "r2")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, ""), Part("l2", 3, "r2")))
    println("ex 10.10 wcMonoid: " + wcMonoid.op(Part("l1", 2, ""), Part("", 3, "r2")))

    //    println("ex 10.11 wordCount: " + wordCount("hi helloDFDF gogo 1234"))
    println("ex 10.11 wordCount: " + wordCount("ab cd"))

    val l = List(1, 2, 3)
    val foldableList: Foldable[List] = List2
    println("ex 10.12 List2: " + foldableList.foldRight(l)(0)(_ + _))

    val s = Stream(1, 2, 3)
    println("ex 10.12 Stream: " + Stream2.foldRight(s)(0)(_ + _))

    val o: Option[String] = None
    println("ex 10.14 FoldableOption: " + FoldableOption.foldRight(o)("b")((a, b) => a + b))
  }

}
