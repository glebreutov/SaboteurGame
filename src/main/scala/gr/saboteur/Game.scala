package gr.saboteur

import scala.util.Random

object PlayerRoles extends Enumeration{
  val DWARF, SABOTEUR = Value
}

object DwarfName extends Enumeration {
  val Thorin, Fili, Kili, Balin, Dwalin, Oin, Gloin, Dori, Nori, Ori, Bifur, Bofur, Bambur = Value
}
class Player (val role: PlayerRoles.Value, val id: DwarfName.Value){

  override def toString: String = role + " " + id
}

object Game {
  def start(playersCount: Int): Game = {

    def initPlayers(): Seq[Player] = {
      val shuffledNames = Random.shuffle(DwarfName.values.toList)
      for (i <- 0 to playersCount) yield {
        val role = if (i == 0) PlayerRoles.SABOTEUR else PlayerRoles.DWARF
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
        deck :::= new Card(CardType.BRAKE_PICK) * 3
        deck :::= new Card(CardType.FIX_PICK) * 2
        deck :::= new Card(CardType.BRAKE_LANTERN) * 3
        deck :::= new Card(CardType.FIX_LANTERN) * 2
        deck :::= new Card(CardType.BRAKE_TRUCK) * 3
        deck :::= new Card(CardType.FIX_TRUCK) * 2
        deck :::= new Card(CardType.REVEAL) * 6
        deck :::= new Card(CardType.BOOM) * 3
        deck ::= new Card(CardType.FIX_LANTERN_PICK)
        deck ::= new Card(CardType.FIX_PICK_TRUCK)
        deck ::= new Card(CardType.FIX_LANTERN_TRUCK)

        Random.shuffle(deck)
    }

    val players = initPlayers
    val deck  = initDeck
    new Game(players, deck, DungeonGraph.init(0))
  }
}
class Game (val players: Seq[Player], val deck: Seq[Card], val graph: DungeonGraph){



//  def makeTurn(player: Player, card: Card, pos: (Int, Int) = null, other: Player = null): Boolean ={
//    card.prop match {
//      case
//    }
//  }

}
