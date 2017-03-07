package fpinscala.parallelism.holyeye.nonblocking

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import fpinscala.parallelism.holyeye.nonblocking.Par._

/**
  * Created by younghankim on 2017. 3. 1..
  */

//case class Par[A](value: A) {
//
//}


sealed trait Future[A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

object Par {

  def printT(s: Any) = {
    println(Thread.currentThread.getName() + ": " + s)
  }

  def tWait(s: Int) = {
    Thread.sleep(s)
  }

  type Par[A] = ExecutorService => Future[A]

  //  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) //old
  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: (A) => Unit) = {
        println(Thread.currentThread.getName() + ": unit(" + cb + ")")
        cb(a)
      }
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    val pes: Future[A] = p(es)
    pes {
      printT("run in pes " + this); a => ref.set(a); latch.countDown
    }
    printT("run in pes before await")
    latch.await() //block
    ref.get
  }

  //1. unit으로 감싸면서 지연 처리된다. X
  //2. map2를 받을 때 부터 call by name으로 지연이다.
  //3. ints.size <= 1인 연산에서 unit을 호출해서 지연처리된다. 따라서 2번이 무의미하다.
  //4. fork에서 Async 처리로 끝
  //  def map2[A, B, C](a: Par[A], b: Par[B], timeout: Long = Long.MaxValue, unit: TimeUnit=TimeUnit.MILLISECONDS)(f: (A, B) => C): Par[C] = {
  //    (es: ExecutorService) => {
  //      val af = a(es)
  //      val bf = b(es)
  //      UnitFuture(f(af.get(timeout, unit), bf.get(timeout, unit)))
  //    }
  //  }

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: (C) => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }
        p1(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  /* OLD
  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A]{
      def call(): A = a(es).get
    })
  }
*/

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = {
        printT("fork apply")
        eval(es)({
          //실행만 하고 빠져나온다.
          printT("eval r start");
          a(es)(cb)
//          tWait(1000)
          printT("eval r end")
        })
      }

    }

  def eval(es: ExecutorService)(r: => Unit): Unit = {
    printT("eval start")
    printT("es " + es)
    es.submit(new Callable[Unit] {
      def call = r
    })
  }


  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(a => a.sorted)
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = {
    map2(a, unit())((a, _) => f(a))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  //ex: 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(Nil): Par[List[A]])((a, b) => map2(a, b)(_ :: _))
  }

  //ex: 7.6
  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    //    unit(ps.foldRight(Nil:List[A])((a,b) => if(f(a)) a::b else b)) //오답
    //    val result: List[Par[A]] =
    //      ps.foldRight(Nil:List[Par[A]])((a: A, b: List[Par[A]]) => if(f(a)) unit(a) :: b else b )
    //    sequence(result)
    //정답확인
    val fps: List[Par[List[A]]] = ps.map(asyncF((a) => if (f(a)) List(a) else Nil))
    map(sequence(fps))(a => a.flatten)
  }

}

object SumTest {
  val es = Executors.newFixedThreadPool(10)

  def main(args: Array[String]): Unit = {

//    println(Par.run(es)(unit(10)))
//    printT(Par.run(es)(fork(unit({printT("MAIN IN FORK"); 10}))))
    printT(Par.run(es)(fork(fork(fork(unit({printT("MAIN IN FORK"); 10}))))))
    printT("main:" + es)

    val p = parMap(List.range(1, 100))(math.sqrt(_))
    val x = run(es)(p)
    println(x)

    es.shutdown()
  }

}
