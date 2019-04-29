package example

import java.time.ZonedDateTime
import java.util.UUID
import codecs.ZoneDateTimeCodec._
import example.models.{Organization, Ticket, User}
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

object Search {
  def searchTickets(keyword: String, orgs: List[Organization], users: List[User], tickets: List[Ticket]): List[Ticket] = {
    val filteredOrgIds = orgs.filter(_.search(keyword)).map(_._id)
    val filteredUserIds = users.filter(u => u.search(keyword) ||
      u.organization_id.map(filteredOrgIds.contains).getOrElse(false)
    ).map(_._id)
    tickets.filter(t =>
      t.search(keyword) ||
        t.organization_id.map(filteredOrgIds.contains).getOrElse(false) ||
        t.assignee_id.map(filteredUserIds.contains).getOrElse(false) ||
        filteredUserIds.contains(t.submitter_id)
    )
  }
}
