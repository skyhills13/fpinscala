package fpinscala.state.holyeye

import fpinscala.state.holyeye.State._

/**
  * Created by younghankim on 2017. 2. 27..
  */
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (v, s2) = run(s)
      f(v).run(s2)
    })
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

}

object StateTest {
  def main(args: Array[String]): Unit = {
    println(unit(1).map(x => x + 1).run(1))
    println(unit(1).map(x => x + 1).run(2))
    println(unit(1).run(unit(10).run(20)))
    println(unit(10))
    println(unit(10).run(unit(11)))
    println(unit(10).get)
  }
}
