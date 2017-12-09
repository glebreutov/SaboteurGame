
import gr.saboteur.DungeonGraph.Dot
import gr.saboteur.{Card, CardType, DungeonGraph}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  var graph = DungeonGraph.init(0)
  val sizeP = graph.graph.size
  graph += Dot(1, 0) -> Card.dungeon(top = true, bottom = true, right = true)
  graph.graph.size should be (sizeP + 1)

//  graph += (1, -1) -> Card.dungeon(right = true, left = true)
  graph += Dot(1, 1) -> Card.dungeon(left = true, right = true)
  graph.graph.size should be (sizeP + 2)
  graph.goldFound() should be (false)

  for (i <- 2 to 5) graph += Dot(i, 0) -> Card.dungeon(top = true, bottom = true, left = true)

  graph.goldFound() should be (true)
  graph += Dot(2, 1) -> Card.dungeon(left = true)
}
