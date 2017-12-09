import gr.saboteur._
import org.scalatest.{FlatSpec, Matchers}

class CardCompatibility extends FlatSpec with Matchers{
//  val one = new Card(DUNGEON, top = false, bottom = true, right = true)
//  val two = new Card(DUNGEON, top = true)
//  one.fit(BOTTOM, two) should be (true)
//  one.fit(LEFT, two) should be (true)
//  one.fit(RIGHT, two) should be (false)

  DUNGEON (Direction.directions: _*).fit(Top, DUNGEON(Left, Right)) should be (false)
  DUNGEON(Left , Right ).fit(Bottom, DUNGEON(Top , Bottom )) should be (false)


}
