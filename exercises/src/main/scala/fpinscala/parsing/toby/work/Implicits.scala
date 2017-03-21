package fpinscala.parsing.toby.work

/**
  * Created by toby on 2017-03-21.
  */
object Implicits {
  // implicit 3가지 방법

  case class Name(nm: String) {
    def printName = println(nm)
  }

  implicit def convertToName(x: String) = Name(x)

  def printName(implicit name: Name) = println(name.nm)

  implicit object defaultName extends Name("Soeun")

  def main(args: Array[String]): Unit = {
    val toby: Name = "Toby"   // 1. conversion to an expected type

    "Younghan".printName    // 2. conversion of the receiver of a selection

    printName(implicitly[Name])  // 3. implicit parameter

  }
}
