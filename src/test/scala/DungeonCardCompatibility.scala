import gr.saboteur.CardEffect.DUNGEON
import gr.saboteur.{DungeonCard, Ways}
import org.scalatest.{FlatSpec, Matchers}

class DungeonCardCompatibility extends FlatSpec with Matchers{
  val one = new DungeonCard(DUNGEON, top = false, bottom = true, right = true)
  val two = new DungeonCard(DUNGEON, top = true)
  one.place(Ways.BOTTOM, two) should be (true)
  one.place(Ways.LEFT, two) should be (true)
  one.place(Ways.RIGHT, two) should be (false)
}
