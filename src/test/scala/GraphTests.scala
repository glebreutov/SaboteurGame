import gr.saboteur.CardType.DUNGEON
import gr.saboteur.{CardType, Card, DungeonGraph}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  var graph = DungeonGraph.init(0)
  val sizeP = graph.graph.size
  graph += (1, 0) -> Card.dungeon(top = true, bottom = true)
  graph.graph.size should be (sizeP + 1)

  graph += (1, -1) -> Card.dungeon(right = true)
  graph += (1, 1) -> Card.dungeon(left = true)
  graph.graph.size should be (sizeP + 1)
  graph.goldFound() should be (false)

  for (i <- 2 to 5) graph += (i, 0) -> Card.dungeon(top = true, bottom = true)

  graph.goldFound() should be (true)

}
