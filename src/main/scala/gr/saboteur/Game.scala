package gr.saboteur

import gr.saboteur.DungeonGraph.Dot
import gr.saboteur.rest.HelloWorld.PlayerID

import scala.util.Random


class GameException(str: String) extends RuntimeException(str)

object DwarfName extends Enumeration {
  val Thorin, Fili, Kili, Balin, Dwalin, Oin, Gloin, Dori, Nori, Ori, Bifur, Bofur, Bambur = Value
}

class Role
object DWARF extends Role
object SABOTEUR extends Role
case class Player (id: String, role: Role, hand: Hand, spells: Spells = Set(), revelations: Set[Dot] = Set()){

  def reveal(d: Dot): Player ={
    Player(id, role, hand, spells, revelations + d)
  }

  def add(add: List[Card]): Player = {
    Player(id, role, hand ++ add, spells, revelations)
  }

  def remove(remove: Card): Player = {
    Player(id, role, hand - remove, spells, revelations)
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
    if(spells.size > 2) this else Player(id, role, hand, newSpells, revelations)
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
  def start(plist: List[PlayerID]): Game = {
    var deck  = Cards.deal()
    val saboteur = Random.nextInt(plist.size)
    val players = for (i <- 0 to plist.size) yield {
      val hand = (deck take 6).toSet
      deck = deck drop 6
      if (i == saboteur) Player(plist(i), SABOTEUR, hand) else Player(plist(i), DWARF, hand)
    }

    new Game(players.toList, deck, DungeonGraph.init(0), Nil)
  }


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
class Game (val players: Players, val deck: Cards, val dungeon: DungeonGraph, val turns: Turns){
  def goldFound(): Boolean = dungeon.goldFound()


  def checkCardPresence(card: Card): Boolean ={
    val hand = players.head.hand
    val contains = card match {
      case mc: MapCard => hand.contains(mc) || hand.contains(Cards.upsideDown(mc))
      case _ => hand.contains(card)
    }
    !contains
  }

  def makeTurn(turn: PlayersTurn): (Game, String) = {
    turn.action() match {
      case _: MakeTunnel.type => tunnel(turn)
      case _: CastSpell.type  => castSpell(turn)
      case _: RevealTreasure.type  => reval(turn)
      case _: DestroyTunnel.type  => destroy(turn)
      case _ => pass(turn)

    }
  }

  def tunnel(turn: PlayersTurn): (Game, String) = {
    val current = players.head

    if(current.spells.nonEmpty)
      return (this, "Player has spells on him, so can't build")

    val upadtedGraph = dungeon + (turn.location -> turn.card.asInstanceOf[MapCard])
    if(upadtedGraph == dungeon)
      return (this, "Card doesn't fit")

    endOfTurn(turn, newdungeon = upadtedGraph)
  }

  def castSpell(turn: PlayersTurn): (Game, String) = {


    val victim = turn.victim.spell(turn.card.asInstanceOf[SpellCard])
    if(victim == turn.victim){
      return (this, "Can't cast more this card")
    }

    val updatedPlayers = for(player <- players.tail :+ players.head)
      yield if(player == turn.victim) victim else player

    endOfTurn(turn, newplayers = updatedPlayers)
  }

  def reval(turn: PlayersTurn): (Game, String) = {
    val player = players.head.reveal(turn.location)
    endOfTurn(turn, player :: players.tail)
    //val fx: _ => Player = {player}
  }

  def endOfTurn(turn: PlayersTurn, newplayers: Players = players, newdungeon: DungeonGraph = dungeon): (Game, String) = {
    if(checkCardPresence(turn.card))
      return (this, "Player does not have this card")

    val player = newplayers.head
      .remove(turn.card)
      .add(deck take 1)
    if (newplayers.lengthCompare(players.size) != 0){
      throw new GameException("players count can't be changed during game")
    }

    def skipEmptyHands(plist: Players): Players = {
      if(plist.forall(_.hand.isEmpty) || plist.head.hand.nonEmpty){
        plist
      } else {
        skipEmptyHands(plist.tail :+ plist.head)
      }
    }
    val game = new Game(skipEmptyHands(newplayers.tail :+ player), deck drop 1, newdungeon, turns :+ turn)
    (game, "Ok")
  }

  def destroy(turn: PlayersTurn): (Game, String) = {
    endOfTurn(turn, newdungeon = dungeon - turn.location)
  }

  def pass(turn: PlayersTurn): (Game, String) = {
    endOfTurn(turn)
  }

  def endOfGame(): Boolean = dungeon.goldFound() || (deck.isEmpty && players.forall(p => p.hand.isEmpty))

}


