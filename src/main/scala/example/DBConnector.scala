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
  }

  def getAllRawUsers() = {
    val content = getContentFrom("users")
    parse(content).flatMap(_.as[List[User]])
  }

  def getAllRawTickets() = {
    val content = getContentFrom("tickets")
    parse(content).flatMap(_.as[List[Ticket]])
  }
}
