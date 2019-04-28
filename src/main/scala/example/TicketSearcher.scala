package example

import cats.data.Kleisli
import cats.effect.IO
import example.models._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.implicits._
import scala.io.StdIn.readLine

object TicketSearcher extends App {

  def read[A](f: Interpreter[IO] => IO[A]) = Kleisli[IO, Interpreter[IO], A](f)

  def program: Kleisli[IO, Interpreter[IO], List[FullTicket]] =
    for {
      target <- read(_.getData())
        .onError { case e: Throwable => read(_.log(s"Error fetching data from json file: ${e.getMessage}")) }
      keyword <- read(_.getKeyword())
        .onError { case e: Throwable => read(_.log(s"Error asking keyword: ${e.getMessage}")) }
      searchedFullTickets = Search.searchTickets(keyword, target.orgs, target.users, target.tickets)
        .map(t => FullTicket.fromTicket(t, target.users, target.orgs))
      _ <- read(_.log(searchedFullTickets.asJson.toString()))
    } yield searchedFullTickets

  val interpreter = new Interpreter[IO] {
    def getData() = IO.fromEither(
      for {
        orgs <- DBConnector.getAllOrganizations()
        users <- DBConnector.getAllRawUsers()
        tickets <- DBConnector.getAllRawTickets()
      } yield SearchTarget(orgs, users, tickets))
    def getKeyword() = IO(readLine("What to search?"))
    def log(str: String) = IO(println(str))
  }

  program.run(interpreter).unsafeRunSync()

}
