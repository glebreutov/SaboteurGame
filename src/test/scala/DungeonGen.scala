import gr.saboteur.DungeonGraph.Location
import gr.saboteur._

import scala.util.Random

object DungeonGen extends App{
  def nearDots(pos: Location) = {
      val (row, col) = pos
      List( (row, col - 1),
        (row, col + 1),
        (row - 1, col),
        (row + 1, col)
      )
  }
  var graph: DungeonGraph = DungeonGraph.init(0)
  var cards: List[Card] = Cards.deal().filter(c => c.prop == DUNGEON)
  val TREASURES_LOC = DungeonGraph.TREASURE_DOTS.map(x => (DungeonGraph.GRAPH_HEIGHT, x))
  while (cards.nonEmpty){
    val card = cards.head
    val outs = graph.graph.keys.toList
    val correctOuts = outs.flatMap(o => nearDots(o)).filter(z => graph.fit(z, card))
    if(correctOuts.isEmpty){
      cards = cards.tail :+ card
      //println("card to tail")
    }else {
      val r = new scala.util.Random

      val len = correctOuts.length - 1
      //println(correctOuts, len)
      graph += (correctOuts(if (len > 0) r.nextInt(len) else 0) -> card)
      cards = cards.tail
      printMap(graph.graph)
      //println(cards.map(x => cardToString(x)).reduce((a, b) => a + " " + b))
    }
    //println(cards.length)
  }
  printMap(graph.graph)

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

  def printMap(graph: Map[Location, Card]) {

    val minVal = graph.map(v => v._1._2).min
    val maxVal = graph.map(v => v._1._2).max
    for (i <- 0 to DungeonGraph.GRAPH_HEIGHT){
      val list = graph.filter(p => p._1._1 == i).map(p => (p._1._2, p._2)).toList

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
