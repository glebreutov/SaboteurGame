package gr.saboteur

import gr.saboteur.DungeonGraph.Location

class CardType
  class UnplayableCard extends CardType
    object START extends UnplayableCard
    object GOLD extends UnplayableCard
    object ORE extends UnplayableCard
  class MapCard extends CardType
    object DUNGEON extends MapCard
    object BOOM extends MapCard
    object DEADEND extends MapCard
  class PlayerRelated extends CardType
    object BRAKE_LANTERN extends PlayerRelated
    object FIX_LANTERN extends PlayerRelated
    object BRAKE_TRUCK extends PlayerRelated
    object FIX_TRUCK extends PlayerRelated
    object BRAKE_PICK extends PlayerRelated
    object FIX_PICK extends PlayerRelated
    object FIX_LANTERN_PICK extends PlayerRelated
    object FIX_PICK_TRUCK extends PlayerRelated
    object FIX_LANTERN_TRUCK extends PlayerRelated
  class SpecialCard extends CardType
    object REVEAL extends SpecialCard

class Direction {

}
object TOP extends Direction
object BOTTOM extends Direction
object LEFT extends Direction
object RIGHT extends Direction

object Direction {
  def opposite(g: Direction): Direction = {
    g match {
      case TOP => BOTTOM
      case BOTTOM => TOP
      case LEFT => RIGHT
      case RIGHT => LEFT
    }
  }
}


object Card{
  var idGen: Int = 0

  def dungeon(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    new Card(DUNGEON, top, bottom, left, right)
  }

  def deadend(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    new Card(DEADEND, top, bottom, left, right)
  }

}
class Card(val prop: CardType,
           val top: Boolean = false,
           val bottom: Boolean = false,
           val left: Boolean = false,
           val right: Boolean = false){

  val id = Card.idGen
  Card.idGen += 1
  val ways = Map(LEFT -> left,
  RIGHT -> right,
  TOP -> top,
  BOTTOM -> bottom
  )

  def place(side: Direction, other: Card): Boolean ={
    ways(side) == other.ways(Direction.opposite(side))
  }

  def *(i: Int): List[Card] ={
    val cards = for (_ <- 0 to i) yield new Card(prop, top, bottom, left, right)
    cards.toList
  }

  override def toString: String = id+ " " + prop.toString
}

object DungeonGraph {
  val GRAPH_HEIGHT = 6
  val TREASURE_LOCATIONS = Set(0, -2, 2)

  type Location = (Int, Int)

  def init(goldPos: Int): DungeonGraph = {
    def card(pos: Int): Card = {
      val artifact = if (pos == goldPos) GOLD else ORE
      new Card(artifact, top = true)
    }


    if (!TREASURE_LOCATIONS(goldPos)){
      throw new RuntimeException("gold card should be in (-2, 0, 2)")
    }
    val g = Map((0, 0) -> new Card(START, bottom = true),
      (GRAPH_HEIGHT, -2) -> card(-2),
      (GRAPH_HEIGHT, 0) -> card(0),
      (GRAPH_HEIGHT, 2) -> card(2))

    new DungeonGraph(g)
  }
}
class DungeonGraph (val graph: Map[Location, Card]){


  def fit(pos: Location, card: Card): Boolean ={
    val sbl = neighbors(pos)
    val (row, _) = pos
    lazy val cardsFit = sbl.map(e => card.place(e._1, graph(e._2))).reduceLeft(_ && _)
    !graph.contains(pos) && row > 0 && row <= DungeonGraph.GRAPH_HEIGHT && sbl.nonEmpty && cardsFit
  }

  def +(elt: (Location, Card)): DungeonGraph ={
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


}
