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
      case _ : FIX_TRUCK => spells.filter(d => !d.isInstanceOf[BRAKE_PICK])
      case _ : FIX_PICK => spells.filter(d => !d.isInstanceOf[BRAKE_PICK])
      case _ : FIX_LANTERN_PICK=> spells.filter(d => !d.isInstanceOf[BRAKE_LANTERN] || !d.isInstanceOf[BRAKE_PICK])
      case _ : FIX_LANTERN_TRUCK=> spells.filter(d => !d.isInstanceOf[BRAKE_LANTERN] || !d.isInstanceOf[BRAKE_TRUCK])
      case _ : FIX_PICK_TRUCK=> spells.filter(d => !d.isInstanceOf[BRAKE_PICK] || !d.isInstanceOf[BRAKE_TRUCK])
    }
    if(spells.size > 2) this else Player(id, role, hand, newSpells)
  }
  override def toString: String = id + " spells: " + spells
}


sealed trait TurnType
case object MakeTunnel extends TurnType
case object CastSpell extends TurnType
case object DestroyTunnel extends TurnType
case object Pass extends TurnType
case object RevealTreasure extends TurnType

case class PlayersTurn(card: Card, location: Dot = null, victim: Player = null, upsideDown: Boolean = false){
  def action(): TurnType = {
    card match {
      case _: MapCard if location != null => MakeTunnel
      case _: SpellCard if victim != null => CastSpell
      case _: REVEAL if location != null => RevealTreasure
      case _: BOOM if location != null => DestroyTunnel
      case _ => Pass
    }
  }

  override def toString: String = {

    val loc = if (location != null) location.toString else if (victim != null) victim.toString else ""
    action() + " " + card + " " +loc
  }
}




object Game {

  def start(playersCount: Int): Game = {

    var deck  = Cards.deal()
    val shuffledNames = Random.shuffle(DwarfName.values.toList.map(n => n.toString))
    val players = for (i <- 0 to playersCount) yield {
      val hand = (deck take 6).toSet
      deck = deck drop 6
      if (i == 0) Player(shuffledNames(i), SABOTEUR, hand) else Player(shuffledNames(i), DWARF, hand)
    }

    new Game(players.toList, deck, DungeonGraph.init(0), List())
  }

}
class Game (val players: List[Player], val deck: List[Card], val dungeon: DungeonGraph, val turns: List[PlayersTurn]){

  def checkCardPresence(card: Card): Unit ={
    if(!players.head.hand.contains(card)){
      throw new GameException("Player doesn't have this card")
    }
  }

  def +(turn: PlayersTurn): Game = {
    turn.action() match {
      case _: MakeTunnel.type => tunnel(turn)
      case _: CastSpell.type  => castSpell(turn)
      case _: RevealTreasure.type  => reval(turn)
      case _: DestroyTunnel.type  => destroy(turn)
      case _ => pass(turn)

    }
  }

  def tunnel(turn: PlayersTurn): Game = {
    val current = players.head
    checkCardPresence(turn.card)
    if(current.spells.nonEmpty){
      throw new GameException("Player has spells on him, so can't build")
    }
    val upadtedGraph = dungeon + (turn.location -> turn.card.asInstanceOf[MapCard])
    if(upadtedGraph == dungeon)
      throw new GameException("Card doesn't fit")

    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, upadtedGraph, turns :+ turn)
  }

  def castSpell(turn: PlayersTurn): Game = {
    checkCardPresence(turn.card)

    val victim = turn.victim.spell(turn.card.asInstanceOf[SpellCard])
    if(victim == turn.victim){
      throw new GameException("Can't cast more this card")
    }

    val updatedPlayers = for(player <- players.tail :+ players.head)
      yield if(player == turn.victim) victim else player

    new Game(updatedPlayers.tail :+ updatedPlayers.head.swap(turn.card, deck.head), deck.tail, dungeon, turns :+ turn)
  }

  def reval(turn: PlayersTurn): Game = {
    checkCardPresence(turn.card)
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon, turns :+ turn)
  }

  def destroy(turn: PlayersTurn): Game = {
    checkCardPresence(turn.card)
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon - turn.location, turns :+ turn)
  }

  def pass(turn: PlayersTurn): Game = {
    new Game(players.tail :+ players.head.swap(turn.card, deck.head), deck.tail, dungeon, turns :+ turn)
  }

  def endOfGame() = dungeon.goldFound() || (deck.isEmpty && players.forall(p => p.hand.isEmpty))

}


