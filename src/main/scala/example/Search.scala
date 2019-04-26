package example

import java.time.ZonedDateTime
import java.util.UUID
import codecs.ZoneDateTimeCodec._
import shapeless.{Poly1, Poly2}

object toSearchableString extends Poly1 {
  implicit def caseInt = at[Int](_.toString)
  implicit def caseOptInt = at[Option[Int]](_.map(_.toString).getOrElse(""))
  implicit def caseString = at[String](identity)
  implicit def caseOptString = at[Option[String]](_.getOrElse(""))
  implicit def caseListString = at[List[String]](_.mkString(","))
  implicit def caseUUID = at[UUID](_.toString)
  implicit def caseDate = at[ZonedDateTime](formatter.format)
  implicit def caseOptDate = at[Option[ZonedDateTime]](_.map(formatter.format).getOrElse(""))
  implicit def caseBoolean = at[Boolean](_.toString)
  implicit def caseOptBoolean = at[Option[Boolean]](_.map(_.toString).getOrElse(""))
}

object keywordSearch extends Poly2 {
  implicit def default[T](implicit st: toSearchableString.Case.Aux[T, String]) = {
    at[(Boolean, String), T] { (tuple, t) =>
      val (boo, key) = tuple
      val result = if (key == "") toSearchableString(t) == "" else toSearchableString(t).contains(key)
      (boo || result, key)
    }
  }
}
