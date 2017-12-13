package gr.saboteur

import gr.saboteur.DungeonGraph.Dot
import Cards._

object DungeonGraph {
  val GRAPH_HEIGHT = 6
  val TREASURE_DOTS = Set(0, -2, 2)
  val TREASURES_POINTS = DungeonGraph.TREASURE_DOTS.map(x => Dot(DungeonGraph.GRAPH_HEIGHT, x))

  case class Dot(row: Int, col: Int){
    def unapply(arg: Dot): Option[(Int, Int)] = Some((arg.row, arg.col))
  }

  def init(goldPos: Int): DungeonGraph = {
    def card(pos: Int): MapCard = {
      if (pos == goldPos) GOLD(Top, Bottom, Left, Right) else ORE(Top, Bottom, Left, Right)

    }

    if (!TREASURE_DOTS(goldPos)){
      throw new RuntimeException("gold card should be in (-2, 0, 2)")
    }
    val tresures = TREASURES_POINTS
        .map(l => l -> card(l.col)).toMap

    new DungeonGraph(tresures + (Dot(0, 0) -> START(Bottom)))
  }
}
class DungeonGraph (val graph: Map[Dot, MapCard]){


  def fit(pos: Dot, card: MapCard): Boolean ={
    val sblPositions = neighbors(pos)
    val Dot(row, _) = pos

    lazy val cardsFit = sblPositions.map(e => card.fit(e._1, graph(e._2))).reduceLeft(_ && _)
    lazy val cardsConnect = sblPositions.map(e => card.connect(e._1, graph(e._2))).reduceLeft(_ || _)
    lazy val tocuhesNumTreasures = sblPositions.values.map(s => DungeonGraph.TREASURES_POINTS(s)).filter(x => x).size
    !graph.contains(pos) && row > 0 && row <= DungeonGraph.GRAPH_HEIGHT && sblPositions.nonEmpty && cardsFit && cardsConnect && tocuhesNumTreasures < sblPositions.size
  }

  def +(elt: (Dot, MapCard)): DungeonGraph = {
    val (pos, card) = elt
    if(fit(pos, card)) new DungeonGraph(graph + (pos -> card)) else this
  }

  def -(pos: Dot): DungeonGraph = {
    new DungeonGraph(graph - pos)
  }

  def neighbors(pos: Dot, g: Map[Dot, Card]=graph.toMap): Map[Direction, Dot] = {
    val Dot(row, col) = pos
    val cards = Map(Left -> Dot(row, col - 1),
      Right -> Dot(row, col + 1),
      Top -> Dot(row - 1, col),
      Bottom -> Dot(row + 1, col)
    )

    cards.filter(e => g.contains(e._2))
  }

  def goldFound(coord : Dot=Dot(0, 0), g: Map[Dot, Card]=graph.toMap): Boolean = {
    g(coord) match {
      case _: DEADEND => false
      case _: GOLD => true
      case _ if g.nonEmpty => neighbors(coord, g).values.map(dot => goldFound(dot, g - coord)).exists(res => res)
      case _ => false
    }
  }




}
