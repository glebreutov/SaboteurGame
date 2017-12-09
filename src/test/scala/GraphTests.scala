
import gr.saboteur.DungeonGraph.Dot
import gr.saboteur.{Bottom, Card, CardType, DUNGEON, DungeonGraph, Top, Left, Right}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  var graph = DungeonGraph.init(0)
  val sizeP = graph.graph.size
  graph += Dot(1, 0) -> Card(DUNGEON, Top , Bottom , Right )
  graph.graph.size should be (sizeP + 1)

//  graph += (1, -1) -> Card(DUNGEON, Right , Left )
  graph += Dot(1, 1) -> Card(DUNGEON, Left , Right )
  graph.graph.size should be (sizeP + 2)
  graph.goldFound() should be (false)

  for (i <- 2 to 5) graph += Dot(i, 0) -> Card(DUNGEON, Top , Bottom , Left )

  graph.goldFound() should be (true)
  graph += Dot(2, 1) -> Card(DUNGEON, Left )
}
