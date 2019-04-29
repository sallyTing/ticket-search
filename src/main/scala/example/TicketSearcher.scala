package example

import cats.data.Kleisli
import cats.effect.IO
import example.models._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.implicits._
import scala.io.Source
import scala.io.StdIn.readLine

object TicketSearcher extends App {

  def read[A](f: Interpreter[IO] => IO[A]) = Kleisli[IO, Interpreter[IO], A](f)

  def program: Kleisli[IO, Interpreter[IO], List[FullTicket]] =
    for {
      orgs <- getData[Organization]("organizations")
      users <- getData[User]("users")
      tickets <- getData[Ticket]("tickets")
      keyword <- read(_.getKeyword())
        .onError { case e: Throwable => read(_.log(s"Error asking keyword: ${e.getMessage}")) }
      searchedFullTickets = Search.searchTickets(keyword, orgs, users, tickets)
        .map(t => FullTicket.fromTicket(t, users, orgs))
      _ <- read(_.log(searchedFullTickets.asJson.toString()))
    } yield searchedFullTickets

  private def getData[A: Searchable](fileName: String): Kleisli[IO, Interpreter[IO], List[A]] =
    for {
      jsonString <- read(_.getJsonContent(fileName))
        .onError { case e: Throwable => read(_.log(s"Error fetching data from json file: ${e.getMessage}")) }
      data <- read(_.getEntities[A](jsonString))
        .onError { case e: Throwable => read(_.log(s"Error parsing json: ${e.getMessage}")) }
    } yield data

  val interpreter = new Interpreter[IO] {
    def getJsonContent(fileName: String): IO[String] = IO {
      val bufferedSource = Source.fromFile(s"data/${fileName}.json")
      val content = (for {
        line <- bufferedSource.getLines
      } yield line).mkString
      bufferedSource.close
      content
    }
    def getEntities[A: Searchable](content: String): IO[List[A]] = IO.fromEither(Searchable.parseFromJsonString[A](content))
    def getKeyword() = IO(readLine("What to search?"))
    def log(str: String) = IO(println(str))
  }

  program.run(interpreter).unsafeRunSync()

}
