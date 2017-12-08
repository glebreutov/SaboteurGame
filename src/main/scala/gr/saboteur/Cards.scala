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
  object TOP extends Direction
  object BOTTOM extends Direction
  object LEFT extends Direction
  object RIGHT extends Direction

object Direction {
  def opposite(g: Direction): Direction = {
    g match {
      case TOP => BOTTOM
      case BOTTOM => TOP
      case LEFT => RIGHT
      case RIGHT => LEFT
    }
  }
}

object Card{
  var idGen: Int = 0

  def dungeon(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    Card(DUNGEON, top, bottom, left, right)
  }

  def deadend(top: Boolean = false, bottom: Boolean = false, left: Boolean = false, right: Boolean = false): Card = {
    Card(DEADEND, top, bottom, left, right)
  }

}
case class Card(prop: CardType,
                top: Boolean = false,
                bottom: Boolean = false,
                left: Boolean = false,
                right: Boolean = false){

  val id = Card.idGen
  Card.idGen += 1
  val ways = Map(LEFT -> left,
    RIGHT -> right,
    TOP -> top,
    BOTTOM -> bottom
  )

  def place(side: Direction, other: Card): Boolean ={
    ways(side) == other.ways(Direction.opposite(side))
  }

  def *(i: Int): List[Card] ={
    val cards = for (_ <- 0 to i) yield new Card(prop, top, bottom, left, right)
    cards.toList
  }

  override def toString: String = id+ " " + prop.toString
}

object Cards {
  def deck: List[Card] = {
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
    deck :::= new Card(BRAKE_PICK) * 3
    deck :::= new Card(FIX_PICK) * 2
    deck :::= new Card(BRAKE_LANTERN) * 3
    deck :::= new Card(FIX_LANTERN) * 2
    deck :::= new Card(BRAKE_TRUCK) * 3
    deck :::= new Card(FIX_TRUCK) * 2
    deck :::= new Card(REVEAL) * 6
    deck :::= new Card(BOOM) * 3
    deck ::= new Card(FIX_LANTERN_PICK)
    deck ::= new Card(FIX_PICK_TRUCK)
    deck ::= new Card(FIX_LANTERN_TRUCK)

    deck
  }
  def deal(): List[Card] = {
    Random.shuffle(Cards.deck)
  }
}
