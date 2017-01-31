package fpinscala.datastructures.toby

/**
  * Toby
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /** Ex 3.25 **/
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /** Ex 3.26 !**/
  def max(t: Tree[Int]): Int =
      t match {
        case Leaf(v) => v
        case Branch(l,r) => max(l).max(max(r))
      }

  /** Ex 3.27 !**/
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(v) => 0
      case Branch(l, r) => (depth(l).max(depth(r))) + 1
    }

  /** Ex 3.28 **/
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /** Ex 3.29 **/
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(v) => l(v)
      case Branch(lt,rt) => b(fold(lt)(l)(b), fold(rt)(l)(b))
    }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def max2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depth2(t: Tree[Int]): Int = fold(t)(_ => 0)(_ max _ + 1)

  /**!! :Tree[B] 캐스팅이 없으면 컴파일이 되지 않음 **/
  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}

object TreeTest {
  def main(args: Array[String]): Unit = {
    import Tree._
    println("ex3.25: " + size(Leaf(1))) // 1
    println("ex3.25: " + size(Branch(Leaf(1), Leaf(2)))) // 3
    println("ex3.25: " + size(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))) // 7

    println("ex3.26: " + max(Leaf(1))) // 1
    println("ex3.26: " + max(Branch(Leaf(1), Leaf(2)))) // 2
    println("ex3.26: " + max(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))) // 7

    println("ex3.27: " + depth(Leaf(1))) // 0
    println("ex3.27: " + depth(Branch(Leaf(1), Leaf(2)))) // 1
    println("ex3.27: " + depth(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))) // 3

    println("ex3.28: " + map(Leaf(1))(_+1))
    println("ex3.28: " + map(Branch(Leaf(1), Leaf(2)))(_*2))
    println("ex3.28: " + map(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))(_-1))

    println("ex3.29-size: " + size2(Leaf(1))) // 1
    println("ex3.29-size: " + size2(Branch(Leaf(1), Leaf(2)))) // 3
    println("ex3.29-size: " + size2(Branch(Leaf(1), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))))) // 7

    println("ex3.29-max: " + max2(Leaf(1))) // 1
    println("ex3.29-max: " + max2(Branch(Leaf(1), Leaf(2)))) // 2
    println("ex3.29-max: " + max2(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))) // 7

    println("ex3.29-depth: " + depth2(Leaf(1))) // 0
    println("ex3.29-depth: " + depth2(Branch(Leaf(1), Leaf(2)))) // 1
    println("ex3.29-depth: " + depth2(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))) // 3

    println("ex3.29-map: " + map2(Leaf(1))(_+1))
    println("ex3.29-map: " + map2(Branch(Leaf(1), Leaf(2)))(_*2))
    println("ex3.29-map: " + map2(Branch(Leaf(1), Branch(Leaf(7), Branch(Leaf(4), Leaf(5)))))(_-1))
  }
}