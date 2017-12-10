package gr.saboteur

import gr.saboteur.DungeonGraph.Dot
import scala.util.Random


class GameException(str: String) extends RuntimeException(str)

object DwarfName extends Enumeration {
  val Thorin, Fili, Kili, Balin, Dwalin, Oin, Gloin, Dori, Nori, Ori, Bifur, Bofur, Bambur = Value
}

class Role
object DWARF extends Role
object SABOTEUR extends Role
case class Player (id: String, role: Role, hand: Set[Card], spells: Set[SpellCard] = Set()){

  def swap(remove: Card, add: Card): Player = {
    val newHand = hand - remove + add
    Player(id, role, newHand, spells)
  }

  def spell(spell: SpellCard): Player = {
    val newSpells = spell match {
      case _ : Curse => spells + spell
      case _ : FIX_LANTERN => spells.filter(d => !d.isInstanceOf[BRAKE_LANTERN])
      case _ : FIX_TRUCK => spells.filter(d => !d.isInstanceOf[FIX_TRUCK])
      case _ : FIX_PICK => spells.filter(d => !d.isInstanceOf[BRAKE_PICK])
      case _ : FIX_LANTERN_PICK=> spells.filter(d => !d.isInstanceOf[BRAKE_LANTERN] || !d.isInstanceOf[BRAKE_PICK])
      case _ : FIX_LANTERN_TRUCK=> spells.filter(d => !d.isInstanceOf[BRAKE_LANTERN] || !d.isInstanceOf[BRAKE_TRUCK])
      case _ : FIX_PICK_TRUCK=> spells.filter(d => !d.isInstanceOf[BRAKE_PICK] || !d.isInstanceOf[BRAKE_TRUCK])
    }
    if(spells.size > 2) this else Player(id, role, hand, newSpells)
  }
  override def toString: String = id + " cards: " + hand
}



class Turn[T<:Card](val card: T)

case class MakeTunnel(override val card: MapCard, location: Dot, upsideDown: Boolean = false) extends Turn[MapCard](card)
case class CastSpell(override val card: SpellCard, victim: Player) extends Turn[SpellCard](card)
case class RevealTreasure(override val card: REVEAL, location: Dot) extends Turn[REVEAL](card)
case class DestroyTunnel(override val card: BOOM, location: Dot) extends Turn[BOOM](card)
case class Pass(override val card: Card) extends Turn[Card](card)


object Game {

  def start(playersCount: Int): Game = {

    var deck  = Cards.deal()
    val shuffledNames = Random.shuffle(DwarfName.values.toList.map(n => n.toString))
    val players = for (i <- 0 to playersCount) yield {
      val hand = (deck take 6).toSet
      deck = deck drop 6
      if (i == 0) Player(shuffledNames(i), SABOTEUR, hand) else Player(shuffledNames(i), DWARF, hand)
    }

    new Game(players.toList, deck, DungeonGraph.init(0))
  }

}
class Game (val players: List[Player], val deck: List[Card], val dungeon: DungeonGraph){

  def checkCardPresence(card: Card): Unit ={
    if(!players.head.hand.contains(card)){
      throw new GameException("Player doesn't have this card")
    }
  }

  def +(turn: MakeTunnel): Game = {
    val current = players.head
    checkCardPresence(turn.card)
    if(current.spells.nonEmpty){
      throw new GameException("Player has spells on him, so can't build")
    }
    val upadtedGraph = dungeon + (turn.location -> turn.card)
    if(upadtedGraph == dungeon)
      throw new GameException("Card doesn't fit")

    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, upadtedGraph)
  }

  def +(turn: CastSpell): Game = {
    checkCardPresence(turn.card)

    val victim = turn.victim.spell(turn.card)
    if(victim == turn.victim){
      throw new GameException("Can't cast more this card")
    }

    val updatedPlayers = for(player <- players.tail :+ players.head)
      yield if(player == turn.victim) victim else player

    new Game(updatedPlayers.tail :+ updatedPlayers.head.swap(turn.card, deck.head), deck.tail, dungeon)
  }

  def +(turn: RevealTreasure): Game = {
    checkCardPresence(turn.card)
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon)
  }

  def +(turn: DestroyTunnel): Game = {
    checkCardPresence(turn.card)
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon - turn.location)
  }

  def +(turn: Pass): Game = {
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon)
  }

  def endOfGame() = dungeon.goldFound() || deck.isEmpty

}


