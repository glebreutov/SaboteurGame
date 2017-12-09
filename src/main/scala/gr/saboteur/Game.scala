package gr.saboteur

import gr.saboteur.DungeonGraph.Dot

import scala.collection.mutable.ListBuffer
import scala.util.Random



object DwarfName extends Enumeration {
  val Thorin, Fili, Kili, Balin, Dwalin, Oin, Gloin, Dori, Nori, Ori, Bifur, Bofur, Bambur = Value
}
abstract class Player (val id: String){
  var cards = ListBuffer[Card]()
  override def toString: String = id + " cards: " + cards
}


case class  DWARF(override val id: String) extends Player(id)
case class  SABOTEUR(override val id: String) extends Player(id)

object Game {

  def start(playersCount: Int): Game = {

    def initPlayers(): List[Player] = {
      val shuffledNames = Random.shuffle(DwarfName.values.toList.map(n => n.toString))
      val players = for (i <- 0 to playersCount)
        yield if (i == 0) SABOTEUR(shuffledNames(i)) else DWARF(shuffledNames(i))

      players.toList
    }



    var deck  = Cards.deal()

    val players = initPlayers()
    for(p <- players){
      p.cards ++= deck take 6
      deck = deck drop 6
    }
    new Game(players, deck, DungeonGraph.init(0))
  }

//  def changePlayer(players: List[Player]): List[Player] = {
//    val player: Player = players.head
//    (players drop 1) :: player :: Nil
//  }
}
class Game (val players: List[Player], val deck: List[Card], val graph: DungeonGraph){

  // operation with card
  // get new card
  // change player

//  def turn(player: Player, card: Card): Game = {
//    //player drop
//    //val players = players take 1
//    new Game()
//  }


  def makeTurn(player: Player, card: Card, locaton: Dot = null, other: Player = null): Game ={

    //make turn

    val changedGraph = graph
    player.cards -= card
    player.cards += deck.head


    val newPlayerList = (players drop 1) :+ player
    new Game(newPlayerList, deck drop 1, changedGraph)
  }

}
