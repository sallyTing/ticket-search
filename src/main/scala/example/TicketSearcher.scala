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
    val filteredOrgs = orgs.filter(_.asJson.noSpaces.contains(keyword))
    val filteredUsers = users.filter(_.asJson.noSpaces.contains(keyword))
    val filteredTickets = tickets.filter(_.asJson.noSpaces.contains(keyword))
    filteredTickets
  }

}

