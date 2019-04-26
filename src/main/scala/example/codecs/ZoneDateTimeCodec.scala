package example
package codecs

import cats.syntax.either._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object ZoneDateTimeCodec {

  val formatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssz")

  implicit val zoneDateTimeEncoder: io.circe.Encoder[ZonedDateTime] =
    io.circe.Encoder.encodeString.contramap[ZonedDateTime](formatter.format)

  implicit val zoneDateTimeDecoder: io.circe.Decoder[ZonedDateTime] =
    io.circe.Decoder.decodeString.emap { str =>
      Either
        .catchNonFatal(ZonedDateTime.parse(str.replaceAll(" ", ""), formatter))
        .leftMap(t => s"cannot decode ZonedDateTime $t")
    }
}