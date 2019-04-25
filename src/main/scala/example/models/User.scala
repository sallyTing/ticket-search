package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import shapeless.Generic
import codecs.ZoneDateTimeCodec._
import io.circe.syntax._

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
) {
  def search(keyword: String): Boolean = {
    val strToSearch = if (keyword == "") ",null," else keyword
    User.unapply(this).get.asJson.noSpaces.contains(strToSearch)
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