package example

import example.models._
import io.circe.generic.auto._
import io.circe.syntax._

object TicketSearcher extends App {

  getAllData() match {
    case Right((organizations, users, tickets)) => {
      val searchedTickets = searchTickets("", organizations, users, tickets)
      println(searchedTickets.map(t => FullTicket.fromTicket(t, users, organizations)).asJson)
    }
    case Left(e) => println(e)
  }

  def getAllData() = {
    for {
      organizations <- DBConnector.getAllOrganizations()
      users <- DBConnector.getAllRawUsers()
      tickets <- DBConnector.getAllRawTickets()
    } yield (organizations, users, tickets)
  }

  def searchTickets(keyword: String, orgs: List[Organization], users: List[User], tickets: List[Ticket]): List[Ticket] = {
    val filteredOrgIds = orgs.filter(_.search(keyword))
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

