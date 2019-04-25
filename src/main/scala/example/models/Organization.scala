package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import codecs.ZoneDateTimeCodec._
import io.circe.syntax._

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
) {
  def search(keyword: String): Boolean = {
    Organization.unapply(this).asJson.noSpaces.contains(keyword)
  }
}

