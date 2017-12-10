package gr.saboteur

import scala.util.Random

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

abstract class Card extends Cloneable {

  def *(i: Int): List[Card] ={

    val cards = for (_ <- 0 to i) yield clone().asInstanceOf[Card]
    cards.toList
  }

  override def toString = this.getClass.getSimpleName
}
abstract class MapCard(val ways: Map[Direction, Boolean]) extends Card {

  def this(tunnels: Direction *){
    this(Direction.directions.map(d => (d, tunnels.contains(d))).toMap)
  }
  def top: Boolean = ways(Top)
  def bottom: Boolean = ways(Bottom)
  def left: Boolean = ways(Left)
  def right: Boolean = ways(Right)

  def connect(side: Direction, other: MapCard): Boolean ={
    ways(side) && fit(side, other)
  }

  def fit(side: Direction, oth: MapCard): Boolean ={
    ways(side) == oth.ways(Direction.opposite(side))
  }
}

case class UpsideDownCard(card: Card) extends MapCard{
  throw new RuntimeException("Implement me")
}


case class DUNGEON(tunnels: Direction *) extends MapCard(tunnels : _*)
case class DEADEND(tunnels: Direction *) extends MapCard(tunnels : _*)
case class START(tunnels: Direction *) extends MapCard(tunnels : _*)
case class GOLD(tunnels: Direction *) extends MapCard(tunnels : _*)
case class ORE(tunnels: Direction *) extends MapCard(tunnels : _*)

class SpellCard extends Card
class Curse extends SpellCard
class Crue extends SpellCard
class  BRAKE_LANTERN extends Curse
class  FIX_LANTERN extends Crue
class  BRAKE_TRUCK extends Curse
class  FIX_TRUCK extends Crue
class  BRAKE_PICK extends Curse
class  FIX_PICK extends Crue
class  FIX_LANTERN_PICK extends Crue
class  FIX_PICK_TRUCK extends Crue
class  FIX_LANTERN_TRUCK extends Crue

class SpecialCard extends Card
class  REVEAL extends SpecialCard
class BOOM extends SpecialCard


object Cards {
  def deck: List[Card] = {
    var deck: List[Card] = DUNGEON(Top, Bottom) * 3
    deck :::= DUNGEON(Top, Bottom, Left, Right) * 5
    deck :::= DUNGEON(Top, Bottom, Left, Right) * 5
    deck :::= DUNGEON(Top, Bottom, Right) * 5
    deck :::= DUNGEON(Bottom, Left) * 4
    deck :::= DUNGEON(Bottom, Left, Right) * 5
    deck :::= DUNGEON(Bottom, Right) * 5
    deck :::= DUNGEON(Left, Right) * 4
    deck ::= DEADEND(Top, Bottom)
    deck ::= DEADEND(Top, Bottom, Left, Right)
    deck ::= DEADEND(Top, Bottom, Right)
    deck ::= DEADEND(Left, Bottom)
    deck ::= DEADEND(Bottom, Left, Right)
    deck ::= DEADEND(Right, Bottom)
    deck ::= DEADEND(Bottom)
    deck ::= DEADEND(Left, Right)
    deck ::= DEADEND(Right)
    deck :::= new BRAKE_PICK() * 3
    deck :::= new FIX_PICK() * 2
    deck :::= new BRAKE_LANTERN() * 3
    deck :::= new FIX_LANTERN() * 2
    deck :::= new BRAKE_TRUCK() * 3
    deck :::= new FIX_TRUCK() * 2
    deck :::= new REVEAL() * 6
    deck :::= new BOOM() * 3
    deck ::= new FIX_LANTERN_PICK()
    deck ::= new FIX_PICK_TRUCK()
    deck ::= new FIX_LANTERN_TRUCK()
    deck
  }
  def deal(): List[Card] = {
    Random.shuffle(Cards.deck)
  }
}
