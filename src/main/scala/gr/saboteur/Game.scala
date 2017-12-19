package gr.saboteur

import gr.saboteur.DungeonGraph.Dot
import gr.saboteur.rest.SabateurWeb.PlayerID

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

  def spell(spell: Card): Player = {
    val newSpells = spell.cardType match {
      case FIX_LANTERN => spells.filter(_.cardType != BRAKE_LANTERN)
      case FIX_TRUCK => spells.filter(_.cardType != BRAKE_TRUCK)
      case FIX_PICK => spells.filter(_.cardType != BRAKE_PICK)
      case FIX_LANTERN_PICK=> spells.find(_.cardType != BRAKE_TRUCK).map(c=> spells - c).getOrElse(spells)
      case FIX_LANTERN_TRUCK=> spells.find(_.cardType != BRAKE_PICK).map(c=> spells - c).getOrElse(spells)
      case FIX_PICK_TRUCK=> spells.find(_.cardType != BRAKE_LANTERN).map(c=> spells - c).getOrElse(spells)
      case _  => spells + spell
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
    card.cardType match {
      case DUNGEON | DEADEND if location != null => MakeTunnel
      case REVEAL if location != null => RevealTreasure
      case BOOM if location != null => DestroyTunnel
      case _ if victim != null => CastSpell
      case _ => Pass
    }
  }

  override def toString: String = {

    val loc = if (location != null) location.toString else if (victim != null) victim.toString else ""
    action() + " " + card + " " +loc
  }
}




object Game {

  val MAX_PLAYERS_PER_GAME = 10

  def start(plist: List[PlayerID]): Game = {
    def chooseSaboteurs(playersCount: Int): Set[Int] = {
      var saboteurs = Set(Random.nextInt(playersCount))
      val sabateursCount = if(playersCount <= 4) 0 else if (playersCount <= 6) 1 else if(playersCount <= 9) 2 else 3
      while (saboteurs.size < sabateursCount){
        saboteurs += Random.nextInt(playersCount)
      }
      saboteurs
    }
    var deck  = Cards.deal()
    val playersCount = plist.length
    val saboteurs = chooseSaboteurs(playersCount)
    val players = for (i <- plist.indices) yield {
      val hand = (deck take cardsPerHand(playersCount)).toSet
      deck = deck drop cardsPerHand(playersCount)
      val role = if (saboteurs.contains(i)) SABOTEUR else DWARF
      Player(plist(i), role, hand)
    }

    val treasurepos = Random.shuffle(List(0, -2, 2)).head
    new Game(players.toList, deck, DungeonGraph.init(treasurepos), Nil)
  }

  def cardsPerHand(playersCount: Int) = {
    if(playersCount <= 5) 6 else if (playersCount <= 7) 5 else 4
  }

  def start(playersCount: Int): Game = {
    val shuffledNames = Random.shuffle(DwarfName.values.toList.map(n => n.toString))

    start(shuffledNames take playersCount)
  }

}
class Game (val players: Players, val deck: Cards, val dungeon: DungeonGraph, val turns: Turns){
  def goldFound(): Boolean = dungeon.goldFound()


  def checkCardPresence(card: Card): Boolean ={
    val hand = players.head.hand
    val contains = card match {
      case mc: DungeonCard => hand.contains(mc) || hand.contains(Cards.upsideDown(mc))
      case _ => hand.contains(card)
    }
    !contains
  }

  def makeTurn(turn: PlayersTurn): (Game, String) = {
    turn.action() match {
      case MakeTunnel => tunnel(turn)
      case CastSpell  => castSpell(turn)
      case RevealTreasure  => reval(turn)
      case DestroyTunnel  => destroy(turn)
      case _ => pass(turn)

    }
  }

  def legitTurn (turn: PlayersTurn): Boolean = {
    makeTurn(turn)._1 != this
  }

  def tunnel(turn: PlayersTurn): (Game, String) = {
    val current = players.head

    if(current.spells.nonEmpty)
      return (this, "Player has spells on him, so can't build")

    val upadtedGraph = dungeon + (turn.location -> turn.card.asInstanceOf[DungeonCard])
    if(upadtedGraph == dungeon)
      return (this, "Card doesn't fit")

    endOfTurn(turn, newdungeon = upadtedGraph)
  }

  def castSpell(turn: PlayersTurn): (Game, String) = {


    val victim = turn.victim.spell(turn.card)
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
    if(!game.players.forall(_.hand.size == Game.cardsPerHand(players.length)) && game.deck.nonEmpty){
      throw new GameException("Wrong card deal")
    }
    (game, "Ok")
  }

  def destroy(turn: PlayersTurn): (Game, String) = {
    endOfTurn(turn, newdungeon = dungeon - turn.location)
  }

  def pass(turn: PlayersTurn): (Game, String) = {
    endOfTurn(turn)
  }

  def gameOver(): Boolean = {
    val emptyHands = players.forall(p => p.hand.isEmpty)
    val everyonePassed = turns.take(players.length).forall(t => t.action() == Pass)
    dungeon.goldFound() || (deck.isEmpty && (emptyHands || everyonePassed))
  }

}


