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
import io.circe.generic.auto._
import io.circe.syntax._
import codecs.ZoneDateTimeCodec._
import scala.io.Source

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
    submitter_id = n % 3,
    assignee_id = if (n > 1) Some(n - 2) else None,
    organization_id = if (n < 3) Some(n % 2) else None
  )

  val invalidOrgs =
    """
      |[
      |  {
      |    "_id": 101,
      |    "url": "http://initech.zendesk.com/api/v2/organizations/101.json",
      |    "external_id": "9270ed79-35eb-4a38-a46f-35725197ea8d",
      |    "name": "Enthaze",
      |    "domain_names": [
      |      "kage.com",
      |      "ecratic.com",
      |      "endipin.com",
      |      "zentix.com"
      |    ],
      |    "created_at": "2016-05-21T11:10",
      |    "details": "MegaCorp",
      |    "shared_tickets": false,
      |    "tags": [
      |      "Fulton",
      |      "West",
      |      "Rodriguez",
      |      "Farley"
      |    ]
      |  }
      |]
    """.stripMargin

  "Result for searching 'username0'" should "return ticket 0, 2, 3" in {
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = {
        fileName match {
          case str if str == "organizations" => IO(orgs.asJson.noSpaces)
          case str if str == "users" => IO(users.asJson.noSpaces)
          case str if str == "tickets" => IO(tickets.asJson.noSpaces)
          case _ => IO.raiseError(new Throwable("invalid input"))
        }
      }
      def getEntities[A: Searchable](content: String) = IO.fromEither(Searchable.parseFromJsonString[A](content))
      def getKeyword() = IO("username0")
      def log(str: String) = IO()
    }
    program.run(interpreter).unsafeRunSync().map(_.subject) shouldBe(List("ticket0", "ticket2", "ticket3"))
  }

  "Result for searching 'organization1'" should "return ticket 1, 3, 4" in {
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = {
        fileName match {
          case str if str == "organizations" => IO(orgs.asJson.noSpaces)
          case str if str == "users" => IO(users.asJson.noSpaces)
          case str if str == "tickets" => IO(tickets.asJson.noSpaces)
          case _ => IO.raiseError(new Throwable("invalid input"))
        }
      }
      def getEntities[A: Searchable](content: String) = IO.fromEither(Searchable.parseFromJsonString[A](content))
      def getKeyword() = IO("organization1")
      def log(str: String) = IO()
    }
    program.run(interpreter).unsafeRunSync().map(_.subject) shouldBe(List("ticket1", "ticket3", "ticket4"))
  }

  "Result for searching empty value" should "return correct result" in {
    val expectedIds = List("4cce7415-ef12-42b6-b7b5-fb00e24f9cc1", "81bdd837-e955-4aa4-a971-ef1e3b373c6d", "5aa53572-b31c-4d27-814b-11709ab00259")
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = IO{
        val bufferedSource = Source.fromFile(s"data/${fileName}.json")
        val content = (for {
          line <- bufferedSource.getLines
        } yield line).mkString
        bufferedSource.close
        content
      }
      def getEntities[A: Searchable](content: String) = {
        val lessTicketsData = Searchable.parseFromJsonString[A](content).map(list => if (list.length == 200) list.take(10) else list)
        IO.fromEither(lessTicketsData)
      }
      def getKeyword() = IO("")
      def log(str: String) = IO()
    }
    program.run(interpreter).unsafeRunSync().map(_._id.toString) shouldBe(expectedIds)
  }

  "Exception when get user input" should "throw exception" in {
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = {
        fileName match {
          case str if str == "organizations" => IO(orgs.asJson.noSpaces)
          case str if str == "users" => IO(users.asJson.noSpaces)
          case str if str == "tickets" => IO(tickets.asJson.noSpaces)
          case _ => IO.raiseError(new Throwable("invalid input"))
        }
      }
      def getEntities[A: Searchable](content: String) = IO.fromEither(Searchable.parseFromJsonString[A](content))
      def getKeyword() = IO.raiseError(new Exception("Error!!"))
      def log(str: String) = IO()
    }
    assertThrows[Exception](program.run(interpreter).unsafeRunSync())
  }

  "Failed to parse json" should "throw exception" in {
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = {
        fileName match {
          case str if str == "organizations" => IO(invalidOrgs)
          case str if str == "users" => IO(users.asJson.noSpaces)
          case str if str == "tickets" => IO(tickets.asJson.noSpaces)
          case _ => IO.raiseError(new Throwable("invalid input"))
        }
      }
      def getEntities[A: Searchable](content: String) = IO.fromEither(Searchable.parseFromJsonString[A](content))
      def getKeyword() = IO("")
      def log(str: String) = IO()
    }
    assertThrows[io.circe.DecodingFailure](program.run(interpreter).unsafeRunSync())
  }

  "Failed to read files" should "throw exception" in {
    val interpreter = new Interpreter[IO] {
      def getJsonContent(fileName: String): IO[String] = IO.raiseError(new Throwable("invalid input"))
      def getEntities[A: Searchable](content: String) = IO.fromEither(Searchable.parseFromJsonString[A](content))
      def getKeyword() = IO("")
      def log(str: String) = IO()
    }
    assertThrows[Throwable](program.run(interpreter).unsafeRunSync())
  }
}
