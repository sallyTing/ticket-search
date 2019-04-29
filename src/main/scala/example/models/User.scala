package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import io.circe.generic.auto._
import codecs.ZoneDateTimeCodec._
import io.circe.parser.parse
import shapeless.Generic

case class User (
                  _id: Int,
                  url: String,
                  external_id: UUID,
                  name: String,
                  alias: Option[String],
                  created_at: ZonedDateTime,
                  active: Boolean,
                  verified: Option[Boolean],
                  shared: Boolean,
                  locale: Option[String],
                  timezone: Option[String],
                  last_login_at: ZonedDateTime,
                  email: Option[String],
                  phone: String,
                  signature: String,
                  organization_id: Option[Int],
                  tags: List[String],
                  suspended: Boolean,
                  role: String
)

object User {
  implicit val userForSearch = new Searchable[User] {
    def search(user: User, keyword: String): Boolean =
      Generic[User].to(user).foldLeft((false, keyword))(keywordSearch)._1

    def parseFromJsonString(jsonString: String): Either[Throwable, List[User]] =
      parse(jsonString).flatMap(_.as[List[User]])
  }
}


case class FullUser (
                  _id: Int,
                  url: String,
                  external_id: UUID,
                  name: String,
                  alias: Option[String],
                  created_at: ZonedDateTime,
                  active: Boolean,
                  verified: Option[Boolean],
                  shared: Boolean,
                  locale: Option[String],
                  timezone: Option[String],
                  last_login_at: ZonedDateTime,
                  email: Option[String],
                  phone: String,
                  signature: String,
                  organization_id: Option[Int],
                  tags: List[String],
                  suspended: Boolean,
                  role: String,
                  organization: Option[Organization]
                )

object FullUser{
  def fromUser(user: User, orgs: List[Organization]): FullUser = {
    Generic[FullUser].from(
      Generic[User].to(user) :+
        user.organization_id.flatMap(id => orgs.find(_._id == id))
    )
  }
}