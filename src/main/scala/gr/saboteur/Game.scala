package gr.saboteur

import gr.saboteur.DungeonGraph.Location

import scala.util.Random
class PlayerRoles
object DWARF extends PlayerRoles
object SABOTEUR extends PlayerRoles


object DwarfName extends Enumeration {
  val Thorin, Fili, Kili, Balin, Dwalin, Oin, Gloin, Dori, Nori, Ori, Bifur, Bofur, Bambur = Value
}
class Player (val role: PlayerRoles, val id: DwarfName.Value){

  override def toString: String = role + " " + id
}

object Game {
  def start(playersCount: Int): Game = {

    def initPlayers(): Seq[Player] = {
      val shuffledNames = Random.shuffle(DwarfName.values.toList)
      for (i <- 0 to playersCount) yield {
        val role = if (i == 0) SABOTEUR else DWARF
        val player = new Player(role, shuffledNames(i))
        player
      }
    }

      def initDeck(): List[Card] = {
        var deck: List[Card] = Card.dungeon(top = true, bottom = true) * 3
        deck :::= Card.dungeon(top = true, bottom = true, left = true, right = true) * 5
        deck :::= Card.dungeon(top = true, bottom = true, left = true, right = true) * 5
        deck :::= Card.dungeon(top = true, bottom = true, right = true) * 5
        deck :::= Card.dungeon(bottom = true, left = true) * 4
        deck :::= Card.dungeon(bottom = true, left = true, right = true) * 5
        deck :::= Card.dungeon(bottom = true, right = true) * 5
        deck :::= Card.dungeon(left = true, right = true) * 4
        deck ::= Card.deadend(top = true, bottom = true)
        deck ::= Card.deadend(top = true, bottom = true, left = true, right = true)
        deck ::= Card.deadend(top = true, bottom = true, right = true)
        deck ::= Card.deadend(left = true, bottom = true)
        deck ::= Card.deadend(bottom = true, left = true, right = true)
        deck ::= Card.deadend(right = true, bottom = true)
        deck ::= Card.deadend(bottom = true)
        deck ::= Card.deadend(left= true, right = true)
        deck ::= Card.deadend(right = true)
        deck :::= new Card(BRAKE_PICK) * 3
        deck :::= new Card(FIX_PICK) * 2
        deck :::= new Card(BRAKE_LANTERN) * 3
        deck :::= new Card(FIX_LANTERN) * 2
        deck :::= new Card(BRAKE_TRUCK) * 3
        deck :::= new Card(FIX_TRUCK) * 2
        deck :::= new Card(REVEAL) * 6
        deck :::= new Card(BOOM) * 3
        deck ::= new Card(FIX_LANTERN_PICK)
        deck ::= new Card(FIX_PICK_TRUCK)
        deck ::= new Card(FIX_LANTERN_TRUCK)

        Random.shuffle(deck)
    }

    val players = initPlayers
    val deck  = initDeck

    new Game(players, deck, DungeonGraph.init(0))
  }

//  def changePlayer(players: List[Player]): List[Player] = {
//    val player: Player = players.head
//    (players drop 1) :: player :: Nil
//  }
}
class Game (val players: Seq[Player], val deck: Seq[Card], val graph: DungeonGraph){


  // operation with card
  // get new card
  // change player

//  def turn(player: Player, card: Card): Game = {
//    //player drop
//    //val players = players take 1
//    new Game()
//  }


  def makeTurn(player: Player, card: Card, locaton: Location = null, other: Player = null): Boolean ={
    card.prop match {
      case DUNGEON => val new_graph = graph + (locaton -> card)
      //case CardType.DUNGEON => val new_graph = graph + (locaton -> card)
    }
  }

}
