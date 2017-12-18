import gr.saboteur.{Cards, DungeonCard, PlayersTurn}
import org.scalatest.{FlatSpec, Matchers}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._, io.circe.generic.semiauto._

class JsonTest extends FlatSpec with Matchers{
//  private val turn = PlayersTurn(Cards.deck.head)
//  println(turn.asJson.noSpaces)

  sealed trait Foo
  // defined trait Foo

  case class Bar(xs: List[String]) extends Foo
  // defined class Bar

  case class Qux(i: Int, d: Option[Double]) extends Foo
  // defined class Qux

  val foo: Foo = Qux(13, Some(14.0))
  // foo: Foo = Qux(13,Some(14.0))

  implicit val DungeonCardDecoder: Decoder[DungeonCard] = deriveDecoder[DungeonCard]
  implicit val DungeonCardEncoder: Encoder[DungeonCard] = deriveEncoder[DungeonCard]

  Cards.GOLD_CARD.asJson
  // res0: String = {"Qux":{"i":13,"d":14.0}}

  //decode[Foo](foo.asJson.spaces4)
}
