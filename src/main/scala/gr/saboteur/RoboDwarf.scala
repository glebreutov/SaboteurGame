package gr.saboteur

import gr.saboteur.DungeonGen.nearDots
import gr.saboteur.DungeonGraph.TREASURES_POINTS

import scala.util.Random

object RoboDwarf {
  //def mapOuts(graph: DungeonGraph) := List[]
  def mapTurns(hand: Set[Card], graph: DungeonGraph): List[MakeTunnel] = {
    val cards = hand.filter(c => c.isInstanceOf[MapCard]).map(c => c.asInstanceOf[MapCard])

    val outs = graph.graph.keys.toList
    outs.flatMap(o => nearDots(o)).flatMap(o => cards.map(c => (o, c)))
      .filter(t => graph.fit(t._1, t._2)).map(t => MakeTunnel(t._2, t._1))
  }

  def mapSpells(hand: Set[Card], players: List[Player]): List[CastSpell] = {
    hand.filter(c => c.isInstanceOf[SpellCard]).map(c => c.asInstanceOf[SpellCard])
      .flatMap(c=> players.filter(p => p != p.spell(c)).map(p=> CastSpell(c, p))).toList
  }

  def mapReveal(hand: Set[Card]): List[RevealTreasure] = {
    hand.filter(c => c.isInstanceOf[REVEAL]).map(c=> c.asInstanceOf[REVEAL])
      .flatMap(c => TREASURES_POINTS.map(p => RevealTreasure(c, p))).toList
  }

  def boomReveal(hand: Set[Card], graph: DungeonGraph): List[DestroyTunnel] = {
    hand.filter(c => c.isInstanceOf[BOOM]).map(c => c.asInstanceOf[BOOM])
      .flatMap(c=> graph.graph.keySet.map(p => DestroyTunnel(c, p))).toList
  }

  def everyTurn(game: Game): List[Turn[_ <: Card]] = {
    val hand = game.players.head.hand
//    val tunnels = mapTurns(hand, game.graph)

    hand.map(c=> Pass(c)).toList ::: mapTurns(hand, game.dungeon) ::: mapSpells(hand, game.players) ::: mapReveal(hand)

  }

  def randomTurn(game: Game): Turn[_ <: Card] = {
    Random.shuffle(everyTurn(game)).head
  }

  def weightMapTurn(t: MakeTunnel, game: Game): Int = {
    //todo: fix it
    //val allMapCards = Cards.deck.filter(c => c.isInstanceOf[MapCard]).map(c => c.asInstanceOf[MapCard])
    //allMapCards - game.dungeon.graph.values.toList
    //calc outs
    0
  }

}
