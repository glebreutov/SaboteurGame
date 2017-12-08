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
    }
    //println(cards.length)
  }
  graph.printMap()
  println()

}
