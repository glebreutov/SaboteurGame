package gr.saboteur

import gr.saboteur.DungeonGen.nearDots
import gr.saboteur.DungeonGraph.TREASURES_POINTS

import scala.util.Random

object RoboDwarf {
  //def mapOuts(graph: DungeonGraph) := List[]
  def mapTurns(game: Game): List[PlayersTurn] = {
    val player = game.players.head
    val hand = player.hand
    val graph = game.dungeon
    val cards = hand.filter(c => player.spells.isEmpty && c.isInstanceOf[MapCard]).map(c => c.asInstanceOf[MapCard])

    val outs = graph.graph.keys.toList
    outs.flatMap(o => nearDots(o)).flatMap(o => cards.map(c => (o, c)))
      .filter(t => graph.fit(t._1, t._2)).map(t => PlayersTurn(t._2, t._1))
  }

  def mapSpells(hand: Set[Card], players: List[Player]): List[PlayersTurn] = {
    hand.filter(c => c.isInstanceOf[SpellCard]).map(c => c.asInstanceOf[SpellCard])
      .flatMap(c=> players.filter(p => p != p.spell(c)).map(p=> PlayersTurn(c, victim = p))).toList
  }

  def mapReveal(hand: Set[Card]): List[PlayersTurn] = {
    hand.filter(c => c.isInstanceOf[REVEAL]).map(c=> c.asInstanceOf[REVEAL])
      .flatMap(c => TREASURES_POINTS.map(p => PlayersTurn(c, p))).toList
  }

  def boomReveal(hand: Set[Card], graph: DungeonGraph): List[PlayersTurn] = {
    hand.filter(c => c.isInstanceOf[BOOM]).map(c => c.asInstanceOf[BOOM])
      .flatMap(c=> graph.graph.keySet.map(p => PlayersTurn(c, p))).toList
  }

  def everyTurn(game: Game): List[PlayersTurn] = {
    val hand = game.players.head.hand
//    val tunnels = mapTurns(hand, game.graph)
//
    (mapSpells(hand, game.players) take 1) :::mapTurns(game) ::: mapReveal(hand) ::: List(PlayersTurn(hand.head))
  }

  def randomTurn(game: Game): PlayersTurn = {
    if(game.players.head.hand.isEmpty)PlayersTurn(null) else Random.shuffle(everyTurn(game)).head
  }

}
