package fpinscala.datastructures.soeun

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /*3.25*/
  def size[A](tree : Tree[A]) : Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /*3.26*/
  def maximum(tree : Tree[Int]) : Int = tree match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /*3.27*/
  def depth[A](tree : Tree[A]) : Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  /*3.28*/
  def map[A, B](tree: Tree[A])(f : A => B) : Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /*3.29*/
//  def fold[A](tree : Tree[A])(f: A => A): Tree[A] = tree match {
//    case Leaf(a) => Leaf(f(a))
//    case Branch(l, r) => Branch(fold(l)(f), fold(r)(f))
//  }
}

object Temp2 {
  def main(args: Array[String]): Unit = {
    import Tree._
    val t1 = Branch(Branch(Leaf(1), Leaf(2)),Leaf(3))
    val t2 = Branch(Branch(Branch(Leaf(13), Leaf(2)),Branch(Leaf(30), Leaf(100))),Leaf(1))
    println(size(t1))
    println(size(t2))

    println(maximum(t1))
    println(maximum(t2))

    println(depth(t1))
    println(depth(t2))

    println(map(t1)(x => x + 1))

  }
}