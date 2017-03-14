package fpinscala.parallelism.soeun

import java.util.concurrent._

import scala.language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  //평가 되지 않은 인수를 Par로 감싸고. 그것을 병렬 평가 대상으로 표시한다.
  //run이 동시적으로 평가할 표현식 a를 감싼다 (130p 참조)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  //얘는 지금 만료 시간을 지키지 않는다.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) //es를 Par에 전달하고
      val bf = b(es) //es를 Par에 전달하고
      //Future의 결과를 기다리고
      //결과에 f에 적용하고, 적용 결과를 UnitFuture로 감쌀 뿐
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  /*7.3
  * Future의 만료 시간 계약을 존중하도록 map2의 구현을 개선*/
  //af의 평가에 걸린 시간을 측정하고 bf의 평가에 걸린 시간에서 그 시간을 빼는 식의 새로운 Future구현이 필요
  def map2Improve[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  /*7.4*/
  def asyncF[A, B](f:A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  //이후에 run이 동시적으로 평가할 계산임을 표시한다.
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /*7.5*/
//  def sequence[A](ps: List[Par[A]]):Par[List[A]] = {
//
//  }

  //sequence를 통해 구현할 수 있는 기능. N개의 병렬 계산을 결합하는 함수
//  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork{
//    val fbs: List[Par[B]] = ps.map(asyncF(f))
//    sequence(fbs)
//  }

  /*7.6*/
//  def parFilter[A](as : List[A])(f: A => Boolean): Par[List[A]] ={
//    //TODO 형만 맞추면 되 그럼 거의 정답이야!!!!!
//    val ff: A => List[A] = {
//      a => if(f(a)) List(a) else List()
//    }
//    as.map(asyncF(ff))
//  }

  /*7.7*/
  /*7.8*/
  /*7.9*/
  /*7.10*/
  //망해버린것이다. 하하하핳하하핳 멘붕이다

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /*7.11*/
  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = {
    es => {
      val index = run(es)(p).get
      run(es)(ps(index))
    }
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
//    choiceN(map(cond)(cond => if(cond) 1 else 0))(List(t, f))
    //    val p: Par[Int] = cond => if(cond) 1 else 0
    val p: Boolean => Int = a => if(a) 0 else 1
    choiceN(map(cond)(p))(List(t, f))
  }

  /*7.12*/
  def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] = {
//    es => Future[A]
    es => {
      val a = run(es)(p) //Future 반환
      val b = a.get() // Context를 벗긴 값 반환
      run(es)(ps(b))
    }
  }

  /*7.13*/
  // see `Nonblocking.scala` answers file. This function is usually called something else!
  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      val k = run(es)(p).get
      run(es)(f(k))
    }
  }

  //chooser가 결국 flatMap 인거라
  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      val k = run(es)(p).get
      run(es)(f(k))
    }
  }

  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] = {
    flatMap(p)(a => if (a) t else f)
  }

  def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] = {
    flatMap(p)(index => choices(index))
  }

  /*7.14*/
  def join[A](p: Par[Par[A]]): Par[A] = {
    es => {
      val a = run(es)(p).get()
      run(es)(a)
    }
  }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(b => b)
  }

//  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = {
//    //TODO 이것은 더 생각해봐야겠구만
//  }


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  /*7.1
  * map2의 서명을 작성하라*/
  //  def map2[A,B](a: Par[A], b: Par[A])(f: (A,A) => B): Par[B] = sys.error("signature")
  //  def map2[A,B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = sys.error("signature")

  /*7.2
  * Par의 표현 고안
  * 6장에서 Rand를 정의했던 것과 같은 것을 정의하라는 뜻*/
  //type Rand[+A] = RNG => (A, RNG)
  //Par는 context를 씌운 것이라고 생각하면 되는 듯
//  type Par[A] = A =>

}
