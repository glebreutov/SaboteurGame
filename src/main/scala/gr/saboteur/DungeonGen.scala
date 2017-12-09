package gr.saboteur

import gr.saboteur.DungeonGraph.Dot

object DungeonGen extends App{
  def nearDots(pos: Dot) = {
      val Dot(row, col) = pos
      List( Dot(row, col - 1),
        Dot(row, col + 1),
        Dot(row - 1, col),
        Dot(row + 1, col)
      )
  }
  var graph: DungeonGraph = DungeonGraph.init(0)
  var cards: List[MapCard] = Cards.deal().filter(c => c.isInstanceOf[DUNGEON]).map(_.asInstanceOf[DUNGEON])
  val TREASURES_LOC = DungeonGraph.TREASURE_DOTS.map(x => (DungeonGraph.GRAPH_HEIGHT, x))
  while (cards.nonEmpty){
    val MapCard = cards.head
    val outs = graph.graph.keys.toList
    val correctOuts = outs.flatMap(o => nearDots(o)).filter(z => graph.fit(z, MapCard))
    if(correctOuts.isEmpty){
      cards = cards.tail :+ MapCard
      //println("MapCard to tail")
    }else {
      val r = new scala.util.Random

      val len = correctOuts.length - 1
      //println(correctOuts, len)
      graph += (correctOuts(if (len > 0) r.nextInt(len) else 0) -> MapCard)
      cards = cards.tail
      printMap(graph.graph)
      //println(cards.map(x => cardToString(x)).reduce((a, b) => a + " " + b))
    }
    //println(cards.length)
  }
  printMap(graph.graph)

  def cardToString(c: MapCard): String ={
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
    }else if(c.top && c.bottom && !c.left && !c.right){
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

  def printMap(graph: Map[Dot, MapCard]) {

    val minVal = graph.map(v => v._1.col).min
    val maxVal = graph.map(v => v._1.col).max
    for (i <- 0 to DungeonGraph.GRAPH_HEIGHT){
      val list = graph.filter(p => p._1.row == i).map(p => (p._1.col, p._2)).toList

      for (j <- minVal to maxVal){
        val tuples = list.filter(q => q._1 == j)
        if(tuples.nonEmpty){
          print(cardToString(tuples.head._2))
        }else {
          print(" ")
        }
      }

      println()
    }
    println()
    println()
  }

}
