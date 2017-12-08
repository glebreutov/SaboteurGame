package gr.saboteur

import gr.saboteur.DungeonGraph.Location
import Cards._

object DungeonGraph {
  val GRAPH_HEIGHT = 6
  val TREASURE_LOCATIONS = Set(0, -2, 2)

  type Location = (Int, Int)

  def init(goldPos: Int): DungeonGraph = {
    def card(pos: Int): Card = {
      val artifact = if (pos == goldPos) GOLD else ORE
      new Card(artifact, top = true, left = true, right = true, bottom = true)
    }

    if (!TREASURE_LOCATIONS(goldPos)){
      throw new RuntimeException("gold card should be in (-2, 0, 2)")
    }
    val tresures = TREASURE_LOCATIONS
      .map(r => (GRAPH_HEIGHT, r))
        .map(l => l -> card(l._2)).toMap

    new DungeonGraph(tresures + ((0, 0) -> new Card(START, bottom = true)))
  }
}
class DungeonGraph (val graph: Map[Location, Card]){


  def fit(pos: Location, card: Card): Boolean ={
    val sbl = neighbors(pos)
    val (row, _) = pos
    lazy val cardsFit = sbl.map(e => card.fit(e._1, graph(e._2))).reduceLeft(_ && _)
    lazy val cardsConnect = sbl.map(e => card.connect(e._1, graph(e._2))).reduceLeft(_ || _)

    !graph.contains(pos) && row > 0 && row <= DungeonGraph.GRAPH_HEIGHT && sbl.nonEmpty && cardsFit && cardsConnect
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

    for((k, v) <- cards if g.contains(v)) yield (k, v)
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

  def printMap() {
    def cardToString(c: Card): String ={
      if(c.top && c.bottom && c.left && c.right){
        return "╬"
      }else if (c.top && c.bottom && c.left){
        return "╣"
      }else if(c.top && c.bottom && c.right){
        return "╠"
      }
      else if(c.bottom && !c.top && !c.right && !c.left){
        return "╥"
      }
      if(c.right && c.left){
        return "═"
      }else if(c.top && c.bottom){
        return "║"
      }else if(c.left && c.bottom){
        return "╗"
      }else if(c.right && c.bottom){
        return "╔"
      }else if(c.left && c.top){
        return "╝"
      }else if(c.right && c.top){
        return "╚"
      }
      throw new RuntimeException("No match")
    }
    val minVal = graph.map(v => v._1._2).min
    val maxVal = graph.map(v => v._1._2).max
    for (i <- 0 to DungeonGraph.GRAPH_HEIGHT){
      val list = graph.filter(p => p._1._1 == i).map(p => (p._1._2, p._2)).toList
      val pad = minVal - (list.map(p => p._1) :+ 0).min.abs

      print(" " * pad)
      for (i <- minVal to maxVal){
        val tuples = list.filter(q => q._1 == i)
        if(tuples.nonEmpty){
          print(cardToString(tuples(0)._2))
        }else {
          print(" ")
        }
      }
      println()
    }
  }


}
