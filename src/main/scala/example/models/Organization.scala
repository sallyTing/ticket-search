package example
package models

import java.time.ZonedDateTime
import java.util.UUID
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
) {
  def search(keyword: String): Boolean =
    Generic[Organization].to(this).foldLeft((false, keyword))(keywordSearch)._1
}

