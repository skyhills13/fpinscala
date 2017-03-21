package fpinscala.parsing.soeun

/**
  * Created by soeunpark on 2017. 3. 19..
  */
trait JSON {
  /*9*/
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }
}
