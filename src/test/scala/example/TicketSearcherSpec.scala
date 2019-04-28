package example

import java.time.ZonedDateTime
import org.scalatest._
import org.scalacheck.Arbitrary
import TicketSearcher._
import cats.effect.IO
import models._
import org.scalacheck.Gen
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Arbitrary._
import java.time.format.DateTimeParseException

class smaTicketSearcherSpec extends FlatSpec with Matchers {

  implicit lazy val arbTime: Arbitrary[ZonedDateTime] = Arbitrary(
    ZonedDateTime.now()
  )

  val orgs = for {
    (org, n) <- Gen.listOfN(2, arbitrary[Organization]).sample.get.zipWithIndex
  } yield org.copy(
    _id = n,
    name = s"organization${n}")

  val users = for {
    (user, n) <- Gen.listOfN(3, arbitrary[User]).sample.get.zipWithIndex
  } yield user.copy(
    _id = n,
    organization_id = Some(n % 2),
    name = s"username${n}")

  val tickets = for {
    (ticket, n) <- Gen.listOfN(5, arbitrary[Ticket]).sample.get.zipWithIndex
  } yield ticket.copy(
    subject = s"ticket${n}",
    description = if (n % 2 == 0) Some(s"description${n}") else None,
    submitter_id = n % 3,
    assignee_id = if (n > 1) Some(n - 2) else None,
    organization_id = if (n < 3) Some(n % 2) else None
  )

  "Result for searching 'username0'" should "return ticket 0, 2, 3" in {
    val interpreter = new Interpreter[IO] {
      def getData() = IO(SearchTarget(orgs, users, tickets))
      def getKeyword() = IO("username0")
      def log(str: String) = IO()
    }
    program.run(interpreter).unsafeRunSync().map(_.subject) shouldBe(List("ticket0", "ticket2", "ticket3"))
  }

  "Result for searching empty value" should "return correct result" in {
    val expectedIds = List("4cce7415-ef12-42b6-b7b5-fb00e24f9cc1", "81bdd837-e955-4aa4-a971-ef1e3b373c6d", "5aa53572-b31c-4d27-814b-11709ab00259")
    val interpreter = new Interpreter[IO] {
      def getData() = IO.fromEither(
        for {
          orgs <- DBConnector.getAllOrganizations()
          users <- DBConnector.getAllRawUsers()
          tickets <- DBConnector.getAllRawTickets()
        } yield SearchTarget(orgs, users, tickets.take(10)))
      def getKeyword() = IO("")
      def log(str: String) = IO()
    }
    program.run(interpreter).unsafeRunSync().map(_._id.toString) shouldBe(expectedIds)
  }

  "Exception when get user input" should "throw exception" in {
    val interpreter = new Interpreter[IO] {
      def getData() = IO(SearchTarget(orgs, users, tickets))
      def getKeyword() = IO.raiseError(new Exception("Error!!"))
      def log(str: String) = IO()
    }
    assertThrows[Exception](program.run(interpreter).unsafeRunSync())
  }

  "Failed to parse json" should "throw exception" in {
    val interpreter = new Interpreter[IO] {
      def getData() = IO.fromEither(
        for {
          orgs <- Left(new DateTimeParseException("","2016-05-21T11:10:28 -10:00", 19))
          users <- Right(users)
          tickets <- Right(tickets)
        } yield SearchTarget(orgs, users, tickets))
      def getKeyword() = IO("")
      def log(str: String) = IO()
    }
    assertThrows[DateTimeParseException](program.run(interpreter).unsafeRunSync())
  }

}
