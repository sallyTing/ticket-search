package example

import example.models._

trait Interpreter[F[_]] {
  def getJsonContent(fileName: String): F[String]
  def getEntities[A: Searchable](content: String): F[List[A]]
  def getKeyword(): F[String]
  def log(str: String): F[Unit]
}
