package gr

package object saboteur {

  type Players = List[Player]
  type Cards = List[Card]
  type Turns = List[PlayersTurn]
  type Hand = Set[Card]
  type Spells = Set[SpellCard]
}
