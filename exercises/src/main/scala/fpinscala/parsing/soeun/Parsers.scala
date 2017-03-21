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
//  //어떤 type에 없는 메서드를 있는 것처럼 사용할 수 있어.
//  //succeed에서 map이 빨간색이지만 실제로 사용할 수 있는 이유가 이것 때문이야.(ParserOps에 map을 정의하면 됨)
//  implicit def asStringParser[A](a: A)(implicit  f:A => Parser[String]): ParserOps[String] = ParserOps(f(a))
//
//  //c //같은 파서가 여러번 되풀이 되는 경우
//  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
////  run(listOfN(3, "ab"| "cd"))("ababcdcd") == Right(List("ab","ab", "cd", "cd"))
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
////  object Laws {
////    def forAll(in: Gen[String]) = ???
////    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))
////    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
////  }
//
////  def char2(c: Char): Parser[Char] =
////    string(c.toString) map (_.charAt(0))
//
//  //Parser는 type이 아니기 때문에 지금 map이 에라나는거야!!! map이라는 메서드가 있을 수 없잖아 ㅋㅋㅋㅋ
//  //ParserOps에다가 map을 구현해야 됨. 그래야 안빨간색.
//  def succeed[A](a: A): Parser[A] = string("") map (_ => a) // 항상 성공해서 알맹이 값 a를 돌려준다. 앞에서 했던 unit과 비슷한 거
//  //Parser는 type이나 클래스가 아니고 type parameter에 불과하기 때문에 간단하게 만들 수 없고 조합기로 만든거야.
//  //type parameter지만 좀 다른거야. 이게 중요해
//
//  def slice[A](p: Parser[A]): Parser[String] // 파싱 성공시 입력 중 p가 조사한 부분을 돌려준다.
////  run(slice(('a'|'b').many))("aaba") == Right("aaba") //many가 누적한 목록은 무시하고 입력 문자열 중 파서와 성공적으로 부합한 부분만 돌려준다.
////  char('a').many.slice.map(_.size) .size가 String의 size이기 때문에 상수 시간에 실행
//  // run(char('a').many.map(_.size))("aaa") == Right(3) 여기서 run의 param부분이 바뀐 것인데, 기껏 List[Char]구축해놓고 길이만 추출하고 폐기하는게 아까워서 도입한거야
//
//  //한 파서를 실행하고 그것이 성공하면 또 다른 파서를 실행하는 수단
//  //Parser[(A, B)] 에서 [여기] 안에 들어있는 것은 A,B두개의 타입이 아니라 두개를 튜플로 묶은 하나의 (A,B)타입임
//  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
//
//  case class ParserOps[A](p: Parser[A]) {
//    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
//    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2) //여기에서 사용하는 self가 trait에서 지정한 self인 듯
//    def product[B>:A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2) // 두 파서를 차례로 실행하고 둘다 성공한 경우에만 그 결과들의 쌍을 돌려준다.
//    def **[B>:A](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
//  }
//
//  /*1*/ //중요하다.
//  //p, p2 파서 두개를 묶고 거기에 f를 적용하고
//  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
//    map(product(p, p2))(f.tupled) //tupled가 붙은 이유는 파라미터 2개를 하나의 tuple파라미터로 바꿔주는 것임
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
//      //재귀는 항상 foldRight로 바꿀 수 있잖아. 토비님 답 봐봐
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
//  /*7*/
//  //p p2의 각 원소를 꺼내서 튜플형태로 만들어서 하나의 파서로
//  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
//    flatMap(p)(a => map(p2)(b => (a,b)))
//  //for {a <- p; b <- p2} yield (a,b) 요렇게 하면 되잖아 ㅠㅠㅠ
//
//  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
//    for { a <- p; b <- p2 } yield f(a,b)
//
////  def label[A](msg: String)(p: Parser[A]): Parser[A]
////
////  def attempt[A](p: Parser[A]): Parser[A] // attempt(p flatMap ( _ => fail)) or p2 == p2 //p의 확정을 p의 성공 이후로 미룬다.
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
