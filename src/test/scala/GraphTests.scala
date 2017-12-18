
import gr.saboteur.DungeonGraph.Dot
import gr.saboteur.{Bottom, Card, DUNGEON, DungeonGraph, Left, DungeonCard, Right, Top}
import org.scalatest.{FlatSpec, Matchers}

class GraphTests extends FlatSpec with Matchers {
  var graph = DungeonGraph.init(0)
  val sizeP = graph.graph.size
  graph += Dot(1, 0) -> DungeonCard.dungeon(Top , Bottom , Right )
  graph.graph.size should be (sizeP + 1)

//  graph += (1, -1) -> Card(DUNGEON, Right , Left )
  graph += Dot(1, 1) -> DungeonCard.dungeon(Left , Right )
  graph.graph.size should be (sizeP + 2)
  graph.goldFound() should be (false)

  for (i <- 2 to 5) graph += Dot(i, 0) -> DungeonCard.dungeon(Top , Bottom , Left )

  graph.goldFound() should be (true)
  graph += Dot(2, 1) -> DungeonCard.dungeon( Left )
}
