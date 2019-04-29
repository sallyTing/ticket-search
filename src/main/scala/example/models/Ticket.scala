package example
package models

import java.time.ZonedDateTime
import java.util.UUID
import io.circe.generic.auto._
import codecs.ZoneDateTimeCodec._
import io.circe.parser.parse
import shapeless.Generic

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
                 )

object Ticket {
  implicit val ticketForSearch = new Searchable[Ticket] {
    def search(ticket: Ticket, keyword: String): Boolean =
      Generic[Ticket].to(ticket).foldLeft((false, keyword))(keywordSearch)._1
    def parseFromJsonString(jsonString: String): Either[Throwable, List[Ticket]] =
      parse(jsonString).flatMap(_.as[List[Ticket]])
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