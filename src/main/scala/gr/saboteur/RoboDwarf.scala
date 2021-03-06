package gr.saboteur

import gr.saboteur.DungeonGen.nearDots
import gr.saboteur.DungeonGraph.TREASURES_POINTS

import scala.util.Random

object RoboDwarf {

  def mapTurns(game: Game): Turns = {
    val player = game.players.head
    val hand = player.hand
    val graph = game.dungeon

    val cards = hand.filter(c => player.spells.isEmpty && c.isInstanceOf[DungeonCard])
      .map(c => c.asInstanceOf[DungeonCard]).toList
//    val cardsUpAndDown = cards ++ cards.map(c => Cards.upsideDown(c))
    val cardsUpAndDown = cards
    val outs = graph.graph.keys.toList
    outs.flatMap(o => nearDots(o)).flatMap(o => cardsUpAndDown.map(c => (o, c)))
      .filter(t => graph.fit(t._1, t._2)).map(t => PlayersTurn(t._2, t._1))
  }

  def mapSpells(hand: Hand, players: Players): Turns = {
    hand.filter(_.cardType.isInstanceOf[SpellCard])
      .flatMap(c=> players.filter(p => p != p.spell(c)).map(p=> PlayersTurn(c, victim = p))).toList
  }

  def mapReveal(hand: Hand): Turns = {
    hand.filter(_.cardType == REVEAL)
      .flatMap(c => TREASURES_POINTS.map(p => PlayersTurn(c, p))).toList
  }

  def boomReveal(hand: Hand, graph: DungeonGraph): Turns = {
    hand.filter(_.cardType == BOOM)
      .flatMap(c=> graph.graph.filter(e => e._2.cardType == DUNGEON || e._2.cardType == DEADEND)
        .keySet.map(p => PlayersTurn(c, p))).toList
  }

  def everyTurn(game: Game): Turns = {
    val hand = game.players.head.hand
//    val tunnels = mapTurns(hand, game.graph)
//
    boomReveal(hand, game.dungeon) ::: (mapSpells(hand, game.players) take 1) :::mapTurns(game) ::: mapReveal(hand) ::: List(PlayersTurn(hand.head))
  }

  def randomTurn(game: Game): PlayersTurn = {
    if(game.players.head.hand.isEmpty)PlayersTurn(null) else Random.shuffle(everyTurn(game)).head
  }

}
