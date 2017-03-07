package fpinscala.parallelism.holyeye

import java.util.concurrent._

import fpinscala.parallelism.holyeye.Par._

/**
  * Created by younghankim on 2017. 3. 1..
  */

//case class Par[A](value: A) {
//
//}

object Par {

  def printT(s: Any) = {
    println(Thread.currentThread.getName() + ": " + s)
  }
  def tWait(s: Int) = {
    Thread.sleep(s)
  }

  type Par[A] = ExecutorService => java.util.concurrent.Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  //old
  private case class UnitFuture[A](get: A) extends java.util.concurrent.Future[A] {
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): java.util.concurrent.Future[A] = a(s) //old

  //1. unit으로 감싸면서 지연 처리된다. X
  //2. map2를 받을 때 부터 call by name으로 지연이다.
  //3. ints.size <= 1인 연산에서 unit을 호출해서 지연처리된다. 따라서 2번이 무의미하다.
  //4. fork에서 Async 처리로 끝
  def map2[A, B, C](a: Par[A], b: Par[B], timeout: Long = Long.MaxValue, unit: TimeUnit=TimeUnit.MILLISECONDS)(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(timeout, unit), bf.get(timeout, unit)))
    }
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A]{
      def call(): A = a(es).get
    })
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(a => a.sorted)
  }

  def map[A,B](a: Par[A])(f: A => B): Par[B] = {
    map2(a, unit())((a,_) => f(a))
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //ex: 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(Nil):Par[List[A]])((a,b) => map2(a,b)(_::_))
  }
  //ex: 7.6
  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
//    unit(ps.foldRight(Nil:List[A])((a,b) => if(f(a)) a::b else b)) //오답
//    val result: List[Par[A]] =
//      ps.foldRight(Nil:List[Par[A]])((a: A, b: List[Par[A]]) => if(f(a)) unit(a) :: b else b )
//    sequence(result)
    //정답확인
    val fps: List[Par[List[A]]] = ps.map(asyncF((a) => if(f(a)) List(a) else Nil))
    map(sequence(fps))(a => a.flatten)
  }

  def equals[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if(run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](cond: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      choices.drop(run(es)(cond).get).head(es)
    }

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      choices.get(run(es)(key).get).get(es)
    }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      choices(pa(es).get)(es)
    }
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(a => if(a) t else f)

  def choiceN2[A](cond: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(cond)(choices)

  def choiceMap2[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices)

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      f(a(es).get)(es)
    }
  }

  def flatMap2[A,B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => {
      val aes: Par[A] = a(es).get()
      aes(es)
    }
  }

  def join2[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(a => a)
  }
}

object SumTest {

  val defaultEs = Executors.newCachedThreadPool()

  def sum1(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def sum2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum2(l) + sum2(r)
    }

  def sum3(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sum1(l))
      val sumR: Par[Int] = unit(sum1(r))
      run(defaultEs)(sumL).get() + run(defaultEs)(sumR).get()
    }

  def sum4(ints: IndexedSeq[Int]): Par[Int] = {
    println(" s4("+ints+")")
    if (ints.size <= 1) {
      unit(ints.headOption getOrElse 0)
    }
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum4(l), sum4(r))(_ + _)
    }
  }

  def sum5(ints: IndexedSeq[Int]): Par[Int] = {
    println(Thread.currentThread.getName() + " s5("+ints+")")
    if (ints.length <= 1)
      lazyUnit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum5(l)), fork(sum5(r)))(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    println("sum1=" + sum1(1.to(4)))
    println("sum2=" + sum2(1.to(4)))
    println("sum3=" + sum3(1.to(4)))
    println("sum4=" + sum4(IndexedSeq(1, 2, 3, 4)))
    println("")
    println("sum5=" + sum5(IndexedSeq(1, 2, 3, 4)))
    println("sum5=" + sum5(IndexedSeq(1, 2, 3, 4))(defaultEs))

    println("Par.unit(1+2)" + unit(1 + 2)(defaultEs))
    println(run(defaultEs)(unit(1 + 2)))

    println("------1")
    val result1: (Int) => Par[Int] = asyncF[Int, Int]((a) => {println(Thread.currentThread.getName() + "|a=" + a); a + 1})
    Thread.sleep(10)
    println("1: " + result1)
    val result2: Par[Int] = result1(1)
    Thread.sleep(10)
    println("2: " + result2)
    val result3: Future[Int] = result2(defaultEs)
    Thread.sleep(10)
    println("3: " + result3)
    println("ex 7.4: " + result3.get())

    println("sortPar: " + sortPar(unit(List(3,2,1)))(defaultEs))
    println("ex7.5: sequence: " + sequence(List(unit(1), unit(2)))(defaultEs))
    println("ex7.5: parMap: " + parMap(List(1,2,3))(_ * 10)(defaultEs))

    println("ex7.6: parFilter: " + parFilter(List(1,2,3,4))(_ % 2 == 0)(defaultEs).get())

    println(map(unit(1))(_ + 1)(defaultEs))
    println(map(unit(1))(_ + 1)(defaultEs) == unit(2)(defaultEs))

    println(Par.equals(defaultEs)(map(unit(1))(_ + 1), unit(2)))

    val x = 10
    def f = (a:Int) => a + 1
    def id[A] = (a: A) => a
    val y = unit(10)

    map(unit(x))(f) == unit(f(x))
    println(Par.equals(defaultEs)(map(unit(x))(f), unit(f(x))))

    map(unit(x))(f) == unit(f(x))
    map(unit(x))(id) == unit(id(x))
    map(unit(x))(id) == unit(x)
    map(y)(id) == y

    println("choice: " + run(defaultEs)(choice(unit(false))(unit(1), unit(2))))
//    println("choice: " + run(defaultEs)(chooser(unit(false))(unit(1), unit(2))))
    println("ex 7. 11 choiceN: " + run(defaultEs)(choiceN(unit(1))(List(unit(0), unit(1), unit(2)))))
    println("ex 7. 12 choiceMap: " + run(defaultEs)( choiceMap(unit(1))(Map(0 -> unit(0), 1 -> unit(100) ))) )

    println("ex 7.13 choicerBoolean: " + run(defaultEs)(choice2(unit(false))(unit(1), unit(2))))
    println("ex 7.13 choicerN: " + run(defaultEs)(choiceN2(unit(1))(List(unit(0), unit(1), unit(2)))))
    println("ex 7.13 choicerMap: " + run(defaultEs)( choiceMap2(unit(1))(Map(0 -> unit(0), 1 -> unit(100) ))) )

    println("ex 7.14 join:" + run(defaultEs)( join(unit(unit(10))) ))
    println("ex 7.14 join:" + run(defaultEs)( join2(unit(unit(10))) ))

    defaultEs.shutdown()
  }
}
