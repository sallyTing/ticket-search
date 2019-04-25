package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import shapeless.Generic
import codecs.ZoneDateTimeCodec._
import io.circe.syntax._

case class Ticket(
                 _id: UUID,
                 url: String,
                 external_id: UUID,
                 created_at: ZonedDateTime,
                 `type`: Option[String],
                 subject: String,
                 description: Option[String],
                 priority: String,
                 status: String,
                 submitter_id: Int,
                 assignee_id: Option[Int],
                 organization_id: Option[Int],
                 tags: List[String],
                 has_incidents: Boolean,
                 due_at: Option[ZonedDateTime],
                 via: String
                 ) {
  def search(keyword: String): Boolean = {
    val strToSearch = if (keyword == "") ",null," else keyword
    Ticket.unapply(this).get.asJson.noSpaces.contains(strToSearch)
  }
}

case class FullTicket(
                   _id: UUID,
                   url: String,
                   external_id: UUID,
                   created_at: ZonedDateTime,
                   `type`: Option[String],
                   subject: String,
                   description: Option[String],
                   priority: String,
                   status: String,
                   submitter_id: Int,
                   assignee_id: Option[Int],
                   organization_id: Option[Int],
                   tags: List[String],
                   has_incidents: Boolean,
                   due_at: Option[ZonedDateTime],
                   via: String,
                   submitter: Option[FullUser],
                   assignee: Option[FullUser],
                   organization: Option[Organization],
                 )

object FullTicket {
  def fromTicket(ticket: Ticket, rawUsers: List[User], orgs: List[Organization]): FullTicket = {
    Generic[FullTicket].from(
      Generic[Ticket].to(ticket) :+
      rawUsers.find(_._id == ticket.submitter_id).map(user => FullUser.fromUser(user, orgs)) :+
      ticket.assignee_id.flatMap(id => rawUsers.find(_._id == id).map(user => FullUser.fromUser(user, orgs))) :+
      ticket.organization_id.flatMap(id => orgs.find(_._id == id))
    )
  }
}