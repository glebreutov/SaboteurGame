import gr.saboteur.CardType.DUNGEON
import gr.saboteur.{Card, Ways}
import org.scalatest.{FlatSpec, Matchers}

class CardCompatibility extends FlatSpec with Matchers{
  val one = new Card(DUNGEON, top = false, bottom = true, right = true)
  val two = new Card(DUNGEON, top = true)
  one.place(Ways.BOTTOM, two) should be (true)
  one.place(Ways.LEFT, two) should be (true)
  one.place(Ways.RIGHT, two) should be (false)
}
