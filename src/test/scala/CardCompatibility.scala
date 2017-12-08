import gr.saboteur._
import org.scalatest.{FlatSpec, Matchers}

class CardCompatibility extends FlatSpec with Matchers{
  val one = new Card(DUNGEON, top = false, bottom = true, right = true)
  val two = new Card(DUNGEON, top = true)
  one.place(BOTTOM, two) should be (true)
  one.place(LEFT, two) should be (true)
  one.place(RIGHT, two) should be (false)
}
