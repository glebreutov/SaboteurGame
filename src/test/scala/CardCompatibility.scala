import gr.saboteur._
import org.scalatest.{FlatSpec, Matchers}

class CardCompatibility extends FlatSpec with Matchers{
//  val one = new Card(DUNGEON, top = false, bottom = true, right = true)
//  val two = new Card(DUNGEON, top = true)
//  one.fit(BOTTOM, two) should be (true)
//  one.fit(LEFT, two) should be (true)
//  one.fit(RIGHT, two) should be (false)

  Card.dungeon(true, true, true, true).fit(TOP, Card.dungeon(left = true, right = true)) should be (false)
  Card.dungeon(left = true, right = true).fit(BOTTOM, Card.dungeon(top = true, bottom = true)) should be (false)


}
