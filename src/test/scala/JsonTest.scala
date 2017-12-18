import gr.saboteur.DungeonGraph.Dot
import gr.saboteur._
import org.scalatest.{FlatSpec, Matchers}


object JsonTest extends App {
//  private val turn = PlayersTurn(Cards.deck.head)
//  println(turn.asJson.noSpaces)
  import io.circe._
  import io.circe.generic.auto._
  import io.circe.parser._
  import io.circe.syntax._, io.circe.generic.semiauto._


//  implicit val DungeonCardDecoder: Decoder[DungeonCard] = deriveDecoder[DungeonCard]
//  implicit val DungeonCardEncoder: Encoder[DungeonCard] = deriveEncoder[DungeonCard]

  println(Dot(0, 0).asJson)
//  println(new Card(BRAKE_LANTERN).asJson)
  //println(Player("dcsdsc", SABOTEUR, Set()))
  // res0: String = {"Qux":{"i":13,"d":14.0}}

  //decode[Foo](foo.asJson.spaces4)
}