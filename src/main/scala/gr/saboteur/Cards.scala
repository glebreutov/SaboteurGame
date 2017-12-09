package gr.saboteur

import scala.util.Random

class CardType {
  override def toString: String = this.getClass.getCanonicalName
}
class MapCard extends CardType
  object DUNGEON extends MapCard
  object BOOM extends MapCard
  object DEADEND extends MapCard
  object START extends MapCard
  object GOLD extends MapCard
  object ORE extends MapCard
class PlayerRelated extends CardType
  object BRAKE_LANTERN extends PlayerRelated
  object FIX_LANTERN extends PlayerRelated
  object BRAKE_TRUCK extends PlayerRelated
  object FIX_TRUCK extends PlayerRelated
  object BRAKE_PICK extends PlayerRelated
  object FIX_PICK extends PlayerRelated
  object FIX_LANTERN_PICK extends PlayerRelated
  object FIX_PICK_TRUCK extends PlayerRelated
  object FIX_LANTERN_TRUCK extends PlayerRelated
class SpecialCard extends CardType
  object REVEAL extends SpecialCard
class PlayerRoles extends CardType
  object DWARF extends PlayerRoles
  object SABOTEUR extends PlayerRoles

class Direction
  object Top extends Direction
  object Bottom extends Direction
  object Left extends Direction
  object Right extends Direction

object Direction {
  val directions = List(Top, Bottom, Left, Right)

  def opposite(g: Direction): Direction = {
    g match {
      case Top => Bottom
      case Bottom => Top
      case Left => Right
      case Right => Left
    }
  }
}

object Card{
  var idGen: Int = 0

}
case class Card(prop: CardType, tunnels: Direction *){

  val id = Card.idGen
  Card.idGen += 1

  val ways: Map[Direction, Boolean] = Direction.directions.map(d => (d, tunnels.contains(d))).toMap

  def top: Boolean = ways(Top)
  def bottom: Boolean = ways(Bottom)
  def left: Boolean = ways(Left)
  def right: Boolean = ways(Right)

  def connect(side: Direction, other: Card): Boolean ={
    ways(side) && fit(side, other)
  }

  def fit(side: Direction, other: Card): Boolean ={
    ways(side) == other.ways(Direction.opposite(side))
  }

  def *(i: Int): List[Card] ={
    val cards = for (_ <- 0 to i) yield new Card(prop, ways.filter(p => p._2).keys.toList: _*)
    cards.toList
  }

  override def toString: String = id + " " + prop.toString
}

object Cards {
  def deck: List[Card] = {
    var deck: List[Card] = Card(DUNGEON, Top, Bottom) * 3
    deck :::= Card(DUNGEON, Top, Bottom, Left, Right) * 5
    deck :::= Card(DUNGEON, Top, Bottom, Left, Right) * 5
    deck :::= Card(DUNGEON, Top, Bottom, Right) * 5
    deck :::= Card(DUNGEON, Bottom, Left) * 4
    deck :::= Card(DUNGEON, Bottom, Left, Right) * 5
    deck :::= Card(DUNGEON, Bottom, Right) * 5
    deck :::= Card(DUNGEON, Left, Right) * 4
    deck ::= Card(DEADEND, Top, Bottom)
    deck ::= Card(DEADEND, Top, Bottom, Left, Right)
    deck ::= Card(DEADEND, Top, Bottom, Right)
    deck ::= Card(DEADEND, Left, Bottom)
    deck ::= Card(DEADEND, Bottom, Left, Right)
    deck ::= Card(DEADEND, Right, Bottom)
    deck ::= Card(DEADEND, Bottom)
    deck ::= Card(DEADEND, Left, Right)
    deck ::= Card(DEADEND, Right)
    deck :::= Card(BRAKE_PICK) * 3
    deck :::= Card(FIX_PICK) * 2
    deck :::= Card(BRAKE_LANTERN) * 3
    deck :::= Card(FIX_LANTERN) * 2
    deck :::= Card(BRAKE_TRUCK) * 3
    deck :::= Card(FIX_TRUCK) * 2
    deck :::= Card(REVEAL) * 6
    deck :::= Card(BOOM) * 3
    deck ::= Card(FIX_LANTERN_PICK)
    deck ::= Card(FIX_PICK_TRUCK)
    deck ::= Card(FIX_LANTERN_TRUCK)

    deck
  }
  def deal(): List[Card] = {
    Random.shuffle(Cards.deck)
  }
}
