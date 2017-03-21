//package fpinscala.parsing.soeun
//
//import fpinscala.testing.SGen
//import fpinscala.testing.answer.{Gen, Prop}
//
//import language.higherKinds
//
////+_에 대한 설명 : +는 내가 아는 그 superClass subClass의 관계이고, _는 새로운 매개변수 타입인데, 어떤 타입이든 상관없는 것
////강의(http://talks.bfpg.org/talks/2016-06-14.fp_in_scala_9_10.html)에 proper type이라는 단어가 등장하는데 솔직히 잘 모르겠음
//
//trait Parsers[Parser[+_]] { self =>
//  //a
//  def char(c: Char): Parser[Char] // 하나의 문자 'a'를 인식하는 파서
//  def run[A](p: Parser[A])(input: String): Either[ParseError, A]  //파서의 실행을 지원하는 함수
//  //run(char(c))(c.toString) == Right(c)
//
//  def orString(s1: String, s2: String): Parser[String] //얘를 리팩토링 해서 아랫 것
//  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
//
//  implicit def string(s: String): Parser[String] //하나의 String을 인식해서 돌려준다.
//
//  //이게 무슨 역할을 하는지 모르겠다.
//  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
//  implicit def asStringParser[A](a: A)(implicit  f:A => Parser[String]): ParserOps[String] = ParserOps(f(a))
//
//  //c //같은 파서가 여러번 되풀이 되는 경우
//  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
//
//  //d로 넘어가기 전 설계에 대해서 생각해볼만 한 점들 193, 194쪽에 나옴. 이 질문들을 가지고 이 후의 설계를 해 나감
//
//  //d => 원래는 ParserOps에 추가해야함
//  def many[A](p: Parser[A]): Parser[List[A]] //p를 실행하고, 그런 다음 p의 파싱이 실패할 때까지 many(p)를 거듭해서 실행한다. 그리고 그 과정에서 성공한 p의 결과를 하나의 목록으로 누적한다.
//  def map[A, B](a: Parser[A])(f: A => B): Parser[B] //map(many(char('a'))).(_.size) // 파싱 성공시 p의 결과에 함수 f를 적용한다.
//
//  //run(char('a').many.map(_.size))("aaa") == Right(3)
//
//  //법칙 검증할 때 사용할 코드
//  object Laws {
//    def forAll(in: Gen[String]) = ???
//    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))
//    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
//  }
//
////  def char2(c: Char): Parser[Char] =
////    string(c.toString) map (_.charAt(0))
//
//  def succeed[A](a: A): Parser[A] = string("") map (_ => a) // 항상 성공해서 값 a를 돌려준다.
//  def slice[A](p: Parser[A]): Parser[String] // 파싱 성공시 입력 중 p가 조사한 부분을 돌려준다.
////  run(slice(('a'|'b').many))("aaba") == Right("aaba") //many가 누적한 목록은 무시하고 입력 문자열 중 파서와 성공적으로 부합한 부분만 돌려준다.
////  char('a').many.slice.map(_.size) .size가 String의 size이기 때문에 상수 시간에 실행
//  // run(char('a').many.map(_.size))("aaa") == Right(3) 여기서 run의 param부분이 바뀐 것인데, 기껏 List[Char]구축해놓고 길이만 추출하고 폐기하는게 아까워서 도입한거야
//
//  //한 파서를 실행하고 그것이 성공하면 또 다른 파서를 실행하는 수단
//  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
//
//  case class ParserOps[A](p: Parser[A]) {
//    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
//    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2) //여기에서 사용하는 self가 trait에서 지정한 self인 듯
//    def product[B>:A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 두 파서를 차례로 실행하고 둘다 성공한 경우에만 그 결과들의 쌍을 돌려준다.
//    def **[B>:A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
//  }
//
//  /*1*/
//  //p, p2 파서 두개를 차례로 실행하고 그 결과에 f를 적용
//  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
//    map(product(p, p2))(f.tupled) //tupled가 붙은 이유는 파라미터가 2개인 함수이기 때문인 듯
//
//  def many1[A](p: Parser[A]): Parser[List[A]] =
//    map2(p, many(p))(_ :: _)
//
//  /*3*/
//  def many2[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
//
//  /*4*/
//  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
//    if (n <= 0) succeed(List())
//    else map2(p, listOfN(n-1, p))(_ :: _)
//
//  //  many(p)
//  //  map2(p, many(p))(_ :: _)
//  //  map2(p, map2(p, many(p))(_ :: _))(_ :: _)
//
//  //문맥 민감 문법을 표현하기 위해 도입한 것이다.
//  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
//  //Parser[A]를 적용한 결과에 대해서 Parser[B]를 적용
//
//
//  /*7*/
//  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
//    flatMap(p)(a => map(p2)(b => (a,b)))
//
//  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
//    for { a <- p; b <- p2 } yield f(a,b)
//
//  def label[A](msg: String)(p: Parser[A]): Parser[A]
//
//  def attempt[A](p: Parser[A]): Parser[A] // attempt(p flatMap ( _ => fail)) or p2 == p2 //p의 확정을 p의 성공 이후로 미룬다.
//
//}
//
//case class Location(input: String, offset: Int = 0) {
//  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
//  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')
//
//  def errorLocation(e: ParseError): Location
//  def errorMeesage(e: ParseError): String
//
//  def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = forAll(inputs ** Gen.string) { case (input, msg) =>
//    run(label(msg)(p))(input) match {
//      case Left(e) => errorMessage(e) == msg
//      case _ => true
//    }
//
//  }
//
//  def toError(msg: String): ParseError =
//    ParseError(List((this, msg)))
//
//  def advanceBy(n: Int) = copy(offset = offset+n)
//
//  def addCommit(isCommitted: Boolean): Result[A] = this match {
//    case Failure(e, c) => Failure(e, c || isCommitted)
//    case _ => this
//  }
//
//  def advanceSuccess(n: Int): Result[A]  = this match {
//    case Success(a, m) => Success(a, n+m)
//    case _ => this
//  }
//
//  /* Returns the line corresponding to this location */
//  def currentLine: String =
//    if (input.length > 1) input.lines.drop(line-1).next
//    else ""
//
//  def scope[A](msg: String)(p: Parser[A]): Parser[A]
//}
//
//case class ParseError(stack: List[(Location,String)] = List(),
//                      otherFailures: List[ParseError] = List()) {
//
//}
//
//
//
//
