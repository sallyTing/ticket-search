package example

import example.models._

trait Interpreter[F[_]] {
  def getData(): F[SearchTarget]
  def getKeyword(): F[String]
  def log(str: String): F[Unit]
}
