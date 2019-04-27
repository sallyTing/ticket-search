package example

import example.models._

trait Interpreter[F[_]] {
  def getData(): F[(List[Organization], List[User], List[Ticket])]
  def getKeyword(): F[String]
  def log(str: String): F[Unit]
}
