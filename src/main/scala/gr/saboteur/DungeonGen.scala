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
  var cards: List[DungeonCard] = Cards.deal().filter(c => c.cardType == DUNGEON).map(_.asInstanceOf[DungeonCard])
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

  def nicePrinter(c: DungeonCard): String ={
    c match {
        case DungeonCard(DUNGEON, Top, Bottom) => "║"
        case Cards.ORE_CARD | DungeonCard(DUNGEON, Top, Bottom, Left, Right) => "╬"
        case DungeonCard(DUNGEON, Top, Bottom, Right) => "╠"
        case DungeonCard(DUNGEON, Bottom, Left) => "╗"
        case Cards.START_CARD | DungeonCard(DUNGEON, Bottom, Left, Right) => "╦"
        case DungeonCard(DUNGEON, Bottom, Right) => "╔"
        case DungeonCard(DUNGEON, Left, Right) => "═"
        case DungeonCard(DEADEND, Top, Bottom) => "│"
        case DungeonCard(DEADEND, Top, Bottom, Left, Right) => "┼"
        case DungeonCard(DEADEND,Top, Bottom, Right) => "├"
        case DungeonCard(DEADEND,Left, Bottom) => "┐"
        case DungeonCard(DEADEND,Bottom, Left, Right) => "┬"
        case DungeonCard(DEADEND,Right, Bottom) => "┌"
        case DungeonCard(DEADEND,Bottom) => "╥"
        case DungeonCard(DEADEND,Left, Right) => "─"
        case DungeonCard(DEADEND,Right) => "╘"
        case _ => "*"
    }
  }

  def printMap(graph: Map[Dot, DungeonCard]) {

    val minVal = graph.map(v => v._1.col).min
    val maxVal = graph.map(v => v._1.col).max
    case class ColAndCard(col: Int, card: DungeonCard)
    for (i <- 0 to DungeonGraph.GRAPH_HEIGHT){
      val list = graph.filter(p => p._1.row == i).map(p => ColAndCard(p._1.col, p._2)).toList

      for (j <- minVal to maxVal){
        val tuples = list.filter(q => q.col == j)
        if(tuples.nonEmpty){
          print(nicePrinter(tuples.head.card))
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
