package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import io.circe.generic.auto._
import codecs.ZoneDateTimeCodec._
import io.circe.parser.parse
import shapeless.Generic

case class Organization (
    _id: Int,
    url: String,
    external_id: UUID,
    name: String,
    domain_names: List[String],
    created_at: ZonedDateTime,
    details: String,
    shared_tickets: Boolean,
    tags: List[String]
)

object Organization {
  implicit val orgForSearch = new Searchable[Organization] {
    def search(org: Organization, keyword: String): Boolean =
      Generic[Organization].to(org).foldLeft((false, keyword))(keywordSearch)._1
    def parseFromJsonString(jsonString: String): Either[Throwable, List[Organization]] =
      parse(jsonString).flatMap(_.as[List[Organization]])
  }

}

