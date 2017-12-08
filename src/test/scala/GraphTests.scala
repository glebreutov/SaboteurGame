import gr.saboteur.CardEffect.DUNGEON
import gr.saboteur.{CardEffect, DungeonCard, DungeonGraph}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  val graph = new DungeonGraph(0)
  graph.add((1, 0), new DungeonCard(DUNGEON, top = true, bottom = true)) should be (true)
  graph.add((1, -1), new DungeonCard(DUNGEON, right = true)) should be (false)
  graph.add((1, 1), new DungeonCard(DUNGEON, left = true)) should be (false)
  graph.goldFound() should be (false)
  graph.add((2, 0), new DungeonCard(DUNGEON, top = true, bottom = true)) should be (true)
  graph.add((3, 0), new DungeonCard(DUNGEON, top = true, bottom = true)) should be (true)
  graph.add((4, 0), new DungeonCard(DUNGEON, top = true, bottom = true)) should be (true)
  graph.add((5, 0), new DungeonCard(DUNGEON, top = true, bottom = true)) should be (true)
  graph.goldFound() should be (true)
}
