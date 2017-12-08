package gr.saboteur

import gr.saboteur.Ways.{BOTTOM, LEFT, RIGHT, TOP}

object CardType extends Enumeration{
  val CardType = Value
  val START, GOLD, ORE, DUNGEON, BOOM, REVEAL, BRAKE_LANTERN,
  FIX_LANTERN, BRAKE_TRUCK, FIX_TRUCK, DEADEND, BRAKE_PICK,
  FIX_PICK, FIX_LANTERN_PICK, FIX_PICK_TRUCK, FIX_LANTERN_TRUCK = Value

}

object Ways extends Enumeration{
  val TOP, BOTTOM, LEFT, RIGHT = Value

  def opposite(g: Ways.Value): Ways.Value ={
    g match {
      case TOP => BOTTOM
      case BOTTOM => TOP
      case LEFT => RIGHT
      case RIGHT => LEFT
    }
  }
}

object Card{
  def dungeon(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    new Card(CardType.DUNGEON, top, bottom, left, right)
  }

  def deadend(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    new Card(CardType.DEADEND, top, bottom, left, right)
  }
}
class Card(val prop: CardType.Value,
           val top: Boolean = false,
           val bottom: Boolean = false,
           val left: Boolean = false,
           val right: Boolean = false){
  val ways = Map(LEFT -> left,
  RIGHT -> right,
  TOP -> top,
  BOTTOM -> bottom
  )

  def place(side: Ways.Value, other: Card): Boolean ={
    ways(side) == other.ways(Ways.opposite(side))
  }

  def *(i: Int): List[Card] ={
    for (_ <- 0 to i) yield new Card(prop, top, bottom, left, right)
  }
}

object DungeonGraph {
  val GRAPH_HEIGHT = 6

  def init(goldPos: Int): DungeonGraph = {
    def card(pos: Int): Card = {
      val artifact = if (pos == goldPos) CardType.GOLD else CardType.ORE
      new Card(artifact, top = true)
    }
    if (!Set(0, -2, 2).contains(goldPos)){
      throw new RuntimeException("gold card should be in (-2, 0, 2)")
    }
    val g = Map((0, 0) -> new Card(CardType.START, bottom = true),
      (GRAPH_HEIGHT, -2) -> card(-2),
      (GRAPH_HEIGHT, 0) -> card(0),
      (GRAPH_HEIGHT, 2) -> card(2))

    new DungeonGraph(g)
  }
}
class DungeonGraph (val graph: Map[(Int, Int), Card]){


  def fit(pos: (Int, Int), card: Card): Boolean ={
    val sbl = neighbors(pos)
    val (row, _) = pos
    lazy val cardsFit = sbl.map(e => card.place(e._1, graph(e._2))).reduceLeft(_ && _)
    card.prop == CardType.DUNGEON && !graph.contains(pos) && row > 0 && row <= DungeonGraph.GRAPH_HEIGHT && sbl.nonEmpty && cardsFit
  }

  def +(elt: ((Int, Int), Card)): DungeonGraph ={
    val (pos, card) = elt
    if(fit(pos, card)){
      return new DungeonGraph(graph + (pos -> card))
    }else if(card.prop == CardType.BOOM){
      return new DungeonGraph(graph - pos)
    }
    this
  }

  def neighbors(pos: (Int, Int), g: Map[(Int, Int), Card]=graph.toMap): Map[Ways.Value, (Int, Int)] = {
    val (row, col) = pos
    val cards = Map(LEFT -> (row, col - 1),
      RIGHT -> (row, col + 1),
      TOP -> (row - 1, col),
      BOTTOM -> (row + 1, col)
    )

    for((k, v) <- cards if g.contains(v)) yield (k, v)
  }

  def goldFound(coord : (Int, Int)=(0, 0), g: Map[(Int, Int), Card]=graph.toMap): Boolean = {
    if(graph.isEmpty){
      return false
    }
    else if(g(coord).prop == CardType.GOLD){
      return true
    }else{
      val sibleings = neighbors(coord, g)
      val graphTail = g - coord
      for((k, v) <- sibleings){
        if(goldFound(v, graphTail)){
          return true
        }
      }
    }
    false
  }


}
