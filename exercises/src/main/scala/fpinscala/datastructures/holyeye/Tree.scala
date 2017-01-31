package fpinscala.datastructures.holyeye

sealed trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

object Tree {

  //Exercise 3.25
  def size[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  //Exercise 3.26
  def maximum(root: Tree[Int]): Int =
    root match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  //Exercise 3.27
  def depth[A](root: Tree[A]): Int =
    root match {
      case Leaf(v) => 1
      case Branch(l, r) => if (depth(l) > depth(r)) depth(l) + 1 else depth(r) + 1
    }

  //Exercise 3.28
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] =
    root match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  //Exercise 3.29
  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = {
    t match {
      case Leaf(v) => l(v)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)((_) => 1)((left, right) => 1 + left + right)

  def maximumFold(t: Tree[Int]): Int = fold(t)((v) => v)((left, right) => left.max(right))

  def depthFold[A](t: Tree[A]): Int = fold(t)((_) => 1)((left, right) => if (left > right) left + 1 else right + 1)

//  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
//    fold(t) ((v) => Leaf(f(v))) ((left, right) => Branch(left, right))

  def main(args: Array[String]) = {
    val root = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val root2 = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    println(root)

    println("==== Exercise 3.25 ====")
    println(size(root))
    println(size(root) == 7)

    println("==== Exercise 3.26 ====")
    println(maximum(root))
    println(maximum(root) == 4)

    println("==== Exercise 3.27 ====")

    println(depth(root))
    println(depth(root) == 3)
    println(root2)
    println(depth(root2) == 4)

    println("==== Exercise 3.28 ====")
    println(map(root)(x => (x * 10)).toString)
    println(map(root)(x => (x * 10).toString) == Branch(Branch(Leaf("10"), Leaf("20")), Branch(Leaf("30"), Leaf("40"))))

    println("==== Exercise 3.29 ====")
    println(sizeFold(root) == 7)
    println(maximumFold(root) == 4)
    println(maximumFold(root))
    println(depthFold(root))
    println(depthFold(root) == 3)

  }


}
