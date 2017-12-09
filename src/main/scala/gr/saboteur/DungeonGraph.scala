package gr.saboteur

import gr.saboteur.DungeonGraph.Location
import Cards._

object DungeonGraph {
  val GRAPH_HEIGHT = 6
  val TREASURE_DOTS = Set(0, -2, 2)
  val TREASURES_POINTS = DungeonGraph.TREASURE_DOTS.map(x => (DungeonGraph.GRAPH_HEIGHT, x))
  type Location = (Int, Int)
  //case class Location(_1: Int, _2: Int)

  def init(goldPos: Int): DungeonGraph = {
    def card(pos: Int): Card = {
      val artifact = if (pos == goldPos) GOLD else ORE
      new Card(artifact, top = true, left = true, right = true, bottom = true)
    }

    if (!TREASURE_DOTS(goldPos)){
      throw new RuntimeException("gold card should be in (-2, 0, 2)")
    }
    val tresures = TREASURE_DOTS
      .map(r => (GRAPH_HEIGHT, r))
        .map(l => l -> card(l._2)).toMap

    new DungeonGraph(tresures + ((0, 0) -> new Card(START, bottom = true)))
  }
}
class DungeonGraph (val graph: Map[Location, Card]){


  def fit(pos: Location, card: Card): Boolean ={
    val sblPositions = neighbors(pos)
    val (row, _) = pos

    lazy val cardsFit = sblPositions.map(e => card.fit(e._1, graph(e._2))).reduceLeft(_ && _)
    lazy val cardsConnect = sblPositions.map(e => card.connect(e._1, graph(e._2))).reduceLeft(_ || _)
    lazy val tocuhesNumTreasures = sblPositions.values.map(s => DungeonGraph.TREASURES_POINTS(s)).filter(x => x).size
    !graph.contains(pos) && row > 0 && row <= DungeonGraph.GRAPH_HEIGHT && sblPositions.nonEmpty && cardsFit && cardsConnect && tocuhesNumTreasures < sblPositions.size
  }

  def +(elt: (Location, Card)): DungeonGraph = {
    val (pos, card) = elt
    if(fit(pos, card)){
      return new DungeonGraph(graph + (pos -> card))
    }else if(card.prop == BOOM){
      return new DungeonGraph(graph - pos)
    }
    this
  }

  def neighbors(pos: Location, g: Map[Location, Card]=graph.toMap): Map[Direction, Location] = {
    val (row, col) = pos
    val cards = Map(LEFT -> (row, col - 1),
      RIGHT -> (row, col + 1),
      TOP -> (row - 1, col),
      BOTTOM -> (row + 1, col)
    )

    cards.filter(e => g.contains(e._2))
  }

  def goldFound(coord : Location=(0, 0), g: Map[Location, Card]=graph.toMap): Boolean = {
    if(graph.isEmpty){
      return false
    }
    else if(g(coord).prop == GOLD){
      return true
    }else{
      val sibleings = neighbors(coord, g)
      val graphTail = g - coord
      for((_, v) <- sibleings){
        if(goldFound(v, graphTail)){
          return true
        }
      }
    }
    false
  }




}
