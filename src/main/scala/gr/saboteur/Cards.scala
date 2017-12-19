package gr.saboteur

import scala.util.Random
import io.circe._
import io.circe.generic.auto._

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
abstract class CardType
object DUNGEON extends CardType
object DEADEND extends CardType
object START extends CardType
object GOLD extends CardType
object ORE extends CardType

class SpellCard extends CardType
class  Curse extends CardType
class  Crue extends CardType
object  BRAKE_LANTERN extends CardType
object  FIX_LANTERN extends CardType
object  BRAKE_TRUCK extends CardType
object  FIX_TRUCK extends CardType
object  BRAKE_PICK extends CardType
object  FIX_PICK extends CardType
object  FIX_LANTERN_PICK extends CardType
object  FIX_PICK_TRUCK extends CardType
object  FIX_LANTERN_TRUCK extends CardType

object SpecialCard extends CardType
object  REVEAL extends CardType
object BOOM extends CardType

class Card(val cardType: CardType) extends Cloneable {
  val id = Card.seq()
  def *(i: Int): List[Card] ={

    val cards = for (_ <- 0 to i) yield new Card(cardType)
    cards.toList
  }

  override def toString: String = id + "-" + cardType.getClass.getSimpleName
}

object Card {
  var counter = 0
  
  def seq() = {
    try{
      counter.toString
    }finally {
      counter +=1
    }
  }
}

case class DungeonCard(override val cardType: CardType, tunnels: Direction *) extends Card(cardType) {
  def top: Boolean = tunnels.contains(Top)
  def bottom: Boolean = tunnels.contains(Bottom)
  def left: Boolean = tunnels.contains(Left)
  def right: Boolean = tunnels.contains(Right)

  def way(direction: Direction): Boolean = {
    direction match {
      case Top => top
      case Bottom => bottom
      case Left => left
      case Right => right
    }
  }
  def connect(side: Direction, other: DungeonCard): Boolean ={
    way(side) && fit(side, other)
  }

  def fit(side: Direction, oth: DungeonCard): Boolean ={
    way(side) == oth.way(Direction.opposite(side)) && oth.cardType != DEADEND
  }

  override def *(i: Int): List[Card] ={
    val cards = for (_ <- 0 to i) yield new DungeonCard(cardType, tunnels :_*)
    cards.toList
  }

  override def equals(obj: scala.Any): Boolean = {

    lazy val other = obj.asInstanceOf[DungeonCard]

    obj.isInstanceOf[DungeonCard] && other.id == id && other.top == top&&
      other.bottom==bottom && other.right == right && other.left==left
  }

  override def toString: String = super.toString + "@" + DungeonGen.nicePrinter(this)
}

object DungeonCard{
  def dungeon(tunnels: Direction *): DungeonCard = {
    DungeonCard(DUNGEON, tunnels :_*)
  }

  def deadend(tunnels: Direction *): DungeonCard ={
    DungeonCard(DEADEND, tunnels :_*)
  }
}




object Cards {

  def upsideDown(card: DungeonCard): DungeonCard = {
    def mirrorParams(card: DungeonCard): List[Direction] = {
      var list:List[Direction] = List()
      if(!card.top)  list :+= Top
      if(!card.bottom)  list :+= Bottom
      if(!card.right)  list :+= Right
      if(!card.left)  list :+= Left
      list
    }
    card.cardType match {
      case DUNGEON => DungeonCard(DUNGEON, mirrorParams(card) :_*)
      case DEADEND => DungeonCard(DEADEND, mirrorParams(card) :_*)
    }
  }

  val ORE_CARD = DungeonCard(ORE, Top, Bottom, Left, Right)
  val GOLD_CARD = DungeonCard(GOLD, Top, Bottom, Left, Right)
  val START_CARD = DungeonCard(START, Bottom)

  val deck: List[Card] = {
    var deck: List[Card] = DungeonCard(DUNGEON, Top, Bottom) * 3
    deck :::= DungeonCard(DUNGEON, Top, Bottom, Left, Right) * 5
    deck :::= DungeonCard(DUNGEON, Top, Bottom, Right) * 5
    deck :::= DungeonCard(DUNGEON, Bottom, Left) * 4
    deck :::= DungeonCard(DUNGEON, Bottom, Left, Right) * 5
    deck :::= DungeonCard(DUNGEON, Bottom, Right) * 5
    deck :::= DungeonCard(DUNGEON, Left, Right) * 4
    deck ::= DungeonCard(DEADEND, Top, Bottom)
    deck ::= DungeonCard(DEADEND, Top, Bottom, Left, Right)
    deck ::= DungeonCard(DEADEND, Top, Bottom, Right)
    deck ::= DungeonCard(DEADEND, Left, Bottom)
    deck ::= DungeonCard(DEADEND, Bottom, Left, Right)
    deck ::= DungeonCard(DEADEND, Right, Bottom)
    deck ::= DungeonCard(DEADEND, Bottom)
    deck ::= DungeonCard(DEADEND, Left, Right)
    deck ::= DungeonCard(DEADEND, Right)
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



