
import gr.saboteur.{CardType, Card, DungeonGraph}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  var graph = DungeonGraph.init(0)
  val sizeP = graph.graph.size
  graph += (1, 0) -> Card.dungeon(top = true, bottom = true, right = true)
  graph.graph.size should be (sizeP + 1)

//  graph += (1, -1) -> Card.dungeon(right = true, left = true)
  graph += (1, 1) -> Card.dungeon(left = true, right = true)
  graph.graph.size should be (sizeP + 2)
  graph.goldFound() should be (false)

  for (i <- 2 to 5) graph += (i, 0) -> Card.dungeon(top = true, bottom = true, left = true)

  graph.goldFound() should be (true)
  graph += (2, 1) -> Card.dungeon(left = true)
}
