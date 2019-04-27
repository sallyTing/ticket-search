package example

import example.models._
import io.circe.parser._
import io.circe.generic.auto._
import codecs.ZoneDateTimeCodec._
import scala.io.Source

object DBConnector {

  def getContentFrom(fileName: String): String = {
    val bufferedSource = Source.fromFile(s"data/${fileName}.json")
    val content = (for {
      line <- bufferedSource.getLines
    } yield line).mkString
    bufferedSource.close
    content
  }

  def getAllOrganizations() = {
    val content = getContentFrom("organizations")
    parse(content).flatMap(_.as[List[Organization]])
      .getOrElse(throw new Throwable("failed to fetch organizations from json file"))
  }

  def getAllRawUsers() = {
    val content = getContentFrom("users")
    parse(content).flatMap(_.as[List[User]])
      .getOrElse(throw new Throwable("failed to fetch users from json file"))
  }

  def getAllRawTickets() = {
    val content = getContentFrom("tickets")
    parse(content).flatMap(_.as[List[Ticket]])
      .getOrElse(throw new Throwable("failed to fetch tickets from json file"))
  }
}
