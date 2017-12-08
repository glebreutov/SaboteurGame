package gr.saboteur




import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CardEffect extends Enumeration{
  val CardEffect = Value
  val START, GOLD, ORE, DUNGEON, BOOM = Value
}

class DungeonCard(val prop: CardEffect.Value,
val top: Boolean = false,
val bottom: Boolean = false,
val left: Boolean = false,
val right: Boolean = false){
  val ways = Map(Ways.LEFT -> left,
  Ways.RIGHT -> right,
  Ways.TOP -> top,
  Ways.BOTTOM -> bottom
  )

  def place(side: Ways.Value, other: DungeonCard): Boolean ={
    ways(side) == other.ways(Ways.opposite(side))
  }
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

class DungeonGraph (goldPos: Int){
  val GRAPH_HEIGHT = 6

  def card(pos: Int): DungeonCard = {
    val artifact = if (pos == goldPos) CardEffect.GOLD else CardEffect.ORE
    new DungeonCard(artifact, top = true)
  }
  if (!Set(0, -2, 2).contains(goldPos)){
    throw new RuntimeException("gold card should be in (-2, 0, 2)")
  }
  val graph = mutable.Map((0, 0) -> new DungeonCard(CardEffect.START, bottom = true),
    (GRAPH_HEIGHT, -2) -> card(-2),
    (GRAPH_HEIGHT, 0) -> card(0),
    (GRAPH_HEIGHT, 2) -> card(2))


  def fit(pos: (Int, Int), card: DungeonCard): Boolean ={
    val sbl = neighbors(pos)
    val (row, _) = pos
    lazy val cardsFit = sbl.map(e => card.place(e._1, graph(e._2))).reduceLeft(_ && _)
    !graph.contains(pos) && row > 0 && row <= GRAPH_HEIGHT && sbl.nonEmpty && cardsFit
  }

  def add(pos: (Int, Int), card: DungeonCard): Boolean ={
    if(fit(pos, card)){
      graph(pos) = card
      return true
    }else if(card.prop == CardEffect.BOOM){
      graph.remove(pos)
      return true
    }
    false
  }

  def neighbors(pos: (Int, Int), g: Map[(Int, Int), DungeonCard]=graph.toMap): Map[Ways.Value, (Int, Int)] = {
    val (row, col) = pos
    val cards = Map(Ways.LEFT -> (row, col - 1),
      Ways.RIGHT -> (row, col + 1),
      Ways.TOP -> (row - 1, col),
      Ways.BOTTOM -> (row + 1, col)
    )

    for((k, v) <- cards if g.contains(v)) yield (k, v)
  }

  def goldFound(coord : (Int, Int)=(0, 0), g: Map[(Int, Int), DungeonCard]=graph.toMap): Boolean = {
    if(graph.isEmpty){
      return false
    }
    else if(g(coord).prop == CardEffect.GOLD){
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
