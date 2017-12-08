package gr.saboteur


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


class DungeonGraph (goldCardNum: Int){
  val GRAPH_HEIGHT = 6
  if (!(goldCardNum ==0 || goldCardNum == -2 || goldCardNum == 2)){
    throw new RuntimeException("gold card should be in (-2, 0, 2)")
  }
  val graph = scala.collection.mutable.Map((0, 0) -> new DungeonCard(CardEffect.START, bottom = true),
    (GRAPH_HEIGHT, -2) -> new DungeonCard(CardEffect.ORE, top = true),
    (GRAPH_HEIGHT, 0) -> new DungeonCard(CardEffect.ORE, top = true),
    (GRAPH_HEIGHT, 2) -> new DungeonCard(CardEffect.ORE, top = true),
  )
  graph((GRAPH_HEIGHT, goldCardNum)) = new DungeonCard(CardEffect.GOLD, top = true)

  def canAdd(pos: (Int, Int), card: DungeonCard): Boolean ={
    val sbl = siblings(pos)
    val (row, col) = pos
    var ok = true
    for((k, v) <- sbl){
      ok = ok && card.place(k, graph(v))
    }

    !graph.contains(pos) && row > 0 && row <= GRAPH_HEIGHT && sbl.nonEmpty && ok
  }

  def add(pos: (Int, Int), card: DungeonCard): Boolean ={
    val can = canAdd(pos, card)
    if(can){
      graph(pos) = card
      return true
    }else if(card.prop == CardEffect.BOOM){
      graph.remove(pos)
      return true
    }
    false
  }

  def siblings(pos: (Int, Int), g: Map[(Int, Int), DungeonCard]=graph.toMap): Map[Ways.Value, (Int, Int)] = {
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
      val sibleings = siblings(coord, g)
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
