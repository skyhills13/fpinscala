package fpinscala.parsing.toby

/**
  * implicit의 3가지 종류
  * Created by toby on 2017-03-18.
  */
object Implicits {
  case class Name(nm: String) {
    def printName = println(nm)
  }

  implicit def stringToName(x: String): Name = Name(x)

  implicit class NameMaker(nm: String) {
    def x(lastNm: String) = Name(nm + " " + lastNm)
  }

  def printName2(implicit nm: Name) = println(nm)

  def main(args: Array[String]): Unit = {
    val n:Name = "Toby" // 1. conversion to an expected type

    "Toby".printName    // 2. conversion of the receiver of a selection
    println("Toby" x "Lee") // implicit class - 컨버젼이 def대신 class 생성자를 통해서 진행되도록

    implicit val defaultName = Name("Default")  // 3. implicit parameter
    printName2


  }
}
