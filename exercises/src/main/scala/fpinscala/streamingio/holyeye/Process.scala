package fpinscala.streamingio.holyeye

/**
  * Created by younghankim on 2017. 5. 15..
  */
sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => {
        val result: Process[I, O] = recv(Some(h))
        result(t)
      }
      case xs => {
        val result: Process[I, O] = recv(None)
        result(xs)
      }
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  //Ex 15.5 (정답참고)
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
    p2 match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(f) => this match {
        case Emit(h, t) => t |> f(Some(h))
        case Halt() => Halt() |> f(None)
        case Await(g) => Await((i: Option[I]) => g(i) |> p2)
      }
    }
  }

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {

  def await[I,O](f: I => Process[I,O],
                 fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }


  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await(recv => recv match {
      case Some(i) => Emit(f(i))
      case None => Halt()
    })

  def filter[I](p: I => Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None => Halt()
      }
    go(0.0)
  }

  //Ex 15.1
  def take[I](n: Int): Process[I,I] = {
    def go(count: Int): Process[I,I] =
      Await {
        case Some(d) if (count < n) => Emit(d, go(count+1))
        case _ => Halt()
      }
    go(0)
  }

  def drop[I](n: Int): Process[I,I] = {
    def go(count: Int): Process[I,I] =
      Await {
        case Some(d) if (count < n) => go(count+1)
        case Some(d) if (count >= n) => Emit(d, go(count+1))
        case None => Halt()
      }
    go(0)
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] = {
    def go(count: Int): Process[I,I] =
      Await {
        case Some(d) if (f(d)) => Emit(d, go(count+1))
        case _ => Halt()
      }
    go(0)
  }

  def dropWhile[I](f: I => Boolean): Process[I,I] = {
    def go(count: Int): Process[I,I] =
      Await {
        case Some(d) if f(d) => go(count+1)
        case Some(d) if !f(d) => Emit(d, go(count+1))
        case None => Halt()
      }
    go(0)
  }

  //Ex 15.2
  def count[I]: Process[I, Int] = {
    def go(accCount: Int): Process[I, Int] =
      Await {
        case Some(d) => Emit(accCount+1, go(accCount+1))
        case None => Halt()
      }
    go(0)
  }

  //Ex 15.3
  def mean: Process[Double, Double] = {
    def go(acc: Double, n: Int): Process[Double, Double] =
      Await {
        case Some(d) => Emit(((d + acc) / n) , go((d + acc), n + 1))
        case None => Halt()
      }
    go(0, 1)
  }

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
    await((i: I) => f(i,z) match {
      case (o,s2) => Emit(o, loop(s2)(f))
    })

  //Ex 15.4
  def sum2: Process[Double, Double] =
    loop(0:Double)((d, acc) => (d+acc, d+acc))

  def count2[I]: Process[I, Int] =
    loop(0)((_, n:Int) => (n+1, n+1))


//  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
//    Await( (i: I) => f(i,z) match {
//      case (o, s2) => Emit(o, loop(s2)(f))
//    })

  def main(args: Array[String]): Unit = {
    val p: Process[Int, Int] = liftOne((x: Int) => x * 10)
    val rp: Process[Int, Int] = p.repeat
    val xs = rp(Stream(1, 2, 3)).toList
    println(xs)
    //    val result: Process[Int, Int] = p.repeat
    //    val x = result(Stream(1,2,3)).toList
    //    println(x)

//    Emit(1).repeat

    val units = Stream.continually(())
    val ones = lift((_:Unit) => 1)(units)
    println(ones.take(10).toList)

    val even = filter((x: Int) => x % 2 == 0)
    val evenResult = even(Stream(1,2,3,4)).toList
    println(evenResult)

    println("sum=" + sum(Stream(1,2,3,4,5)).toList)
    println("sum2=" + sum2(Stream(1,2,3,4,5)).toList)
    println("take=" + take(3)(Stream(1,2,3,4,5)).toList)
    println("drop=" + drop(3)(Stream(1,2,3,4,5)).toList)
    println("takeWhile=" + takeWhile((i: Int) => (i <= 3))(Stream(1,2,3,4,5)).toList)
    println("dropWhile=" + dropWhile((i: Int) => (i <= 3))(Stream(1,2,3,4,5)).toList)
    println("count=" + count(Stream(1,2,3,4,5)).toList)
    println("count2=" + count2(Stream(1,2,3,4,5)).toList)
    println("mean=" + mean(Stream(1,2,3,4,5)).toList)

    println("take+sum" + take(3)(sum(Stream(1,2,3,4,5))).toList)
    println("take+sum" + sum(take(3)(Stream(1,2,3,4,5))).toList)

    println("sum |> take" + (sum |> take(3))(Stream(1,2,3,4,5)).toList)

  }
}
