package example

import cats.data.Kleisli
import cats.effect.IO
import example.models._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.implicits._

object TicketSearcher extends App {

  def read[A](f: Interpreter[IO] => IO[A]) = Kleisli[IO, Interpreter[IO], A](f)

  def program: Kleisli[IO, Interpreter[IO], Unit] =
    for {
      (orgs, users, tickets) <- read(_.getData())
        .onError { case e: Throwable => read(_.log(s"Error fetching data: ${e.getMessage}")) }
      _ <- read(_.log(""))
      keyword <- read(_.getKeyword())
      searchedTickets = Search.searchTickets(keyword, orgs, users, tickets)
      _ <- read(_.log(searchedTickets.map(t => FullTicket.fromTicket(t, users, orgs)).asJson.toString()))
    } yield ()

  val interpreter = new Interpreter[IO] {
    def getData() = IO((
      DBConnector.getAllOrganizations(),
      DBConnector.getAllRawUsers(),
      DBConnector.getAllRawTickets()
    ))
    def getKeyword() = IO("")
    def log(str: String) = IO(println(str))
  }

  program.run(interpreter).unsafeRunSync()

}

